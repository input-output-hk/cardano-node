{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Handlers.Shutdown
  (
  -- * Generalised shutdown handling
    ShutdownFDs
  , withShutdownHandling

  -- * Requesting shutdown
  , ShutdownDoorbell
  , getShutdownDoorbell
  , triggerShutdown

  -- * Watch ChainDB for passing a configured slot sync limit threshold,
  --   translating it to a graceful shutdown.
  , maybeSpawnOnSlotSyncedShutdownHandler
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)

import           Data.Text (pack)

import qualified GHC.IO.Handle.FD as IO (fdToHandle)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           System.Posix.Types (Fd (Fd))
import qualified System.Process as IO (createPipeFd)

import           Cardano.BM.Data.Tracer (TracingVerbosity (..), severityNotice, trTransformer)
import           Cardano.BM.Trace
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Tracer
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (onEachChange)
import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo, pointSlot)

import           Cardano.Node.Types

-- | 'ShutdownFDs' mediate the graceful shutdown requests,
-- either external or internal to the process.
--
-- In the external mediation case, the parent process passes us the file descriptor
-- number of the read end of a pipe, via the CLI with @--shutdown-ipc FD@.
-- In the internal mediation case, we create our own pipe.
-- In both cases we store the accessible ends in 'ShutdownFDs'.
--
-- In either case, if the write end gets closed, either deliberately:
--   - by internal call of 'triggerShutdown' on 'ShutdownFDs', or
--   - by parent process
-- ..or automatically, because the parent process itself terminated,
-- then we initiate a clean shutdown.
data ShutdownFDs
  = NoShutdownFDs
  | ExternalShutdown !ShutdownListener
  -- ^ Extra-processually signalled shutdown.
  | InternalShutdown !ShutdownListener !ShutdownDoorbell
  -- ^ Intra-processually signalled shutdown.

-- | FD used to send an EOF-based request for shutdown.
newtype ShutdownDoorbell = ShutdownDoorbell { _doorbellFd :: Fd }

-- | FD we're listening on for the EOF signalling the shutdown.
newtype ShutdownListener = ShutdownListener { _listenerFd :: Fd }

raceAll :: [IO ()] -> IO ()
raceAll (a:b:as) = race_ a (raceAll (b:as))
raceAll [a] = a
raceAll [] = IO.hPutStrLn IO.stderr "Nothing to race"

-- | Gracefully handle shutdown requests, if requested by 'ShutdownFDs'.
--
-- The file descriptor wrapped in a 'ShutdownListener' designates the
-- receiving end of the shutdown signalling communication channel.
-- The opposite end might be either internal or external to the node process.
withShutdownHandler :: ShutdownFDs -> Maybe FilePath -> Trace IO Text -> IO () -> IO ()
withShutdownHandler sfds maybeShutdownFile trace action = raceAll $ catMaybes
  [ maybeShutdownAction
  , maybeShutdownFileAction
  , Just action
  ]
  where
    maybeShutdownAction :: Maybe (IO ())
    maybeShutdownAction = case sfdsListener sfds of
      Just (ShutdownListener fd) -> Just (wrapUninterruptableIO $ waitForFdEOF fd)
      Nothing -> Nothing

    maybeShutdownFileAction :: Maybe (IO ())
    maybeShutdownFileAction = case maybeShutdownFile of
      Just filePath -> if filePath == "-"
        then Just (waitForHandleEOF IO.stdin)
        else Just (waitForHandleEOF =<< IO.openFile filePath ReadMode)
      Nothing -> Nothing

    waitForHandleEOF :: Handle -> IO ()
    waitForHandleEOF h = do
      r <- try $ IO.hGetChar h
      case r of
        Left e
          | IO.isEOFError e -> traceWith tracer "received shutdown request"
          | otherwise       -> throwIO e

        Right _  ->
          throwIO $ IO.userError "--shutdown-ipc FD does not expect input"

    waitForFdEOF :: Fd -> IO ()
    waitForFdEOF (Fd fd) = IO.fdToHandle fd >>= waitForHandleEOF

    sfdsListener :: ShutdownFDs -> Maybe ShutdownListener
    sfdsListener = \case
      ExternalShutdown r -> Just r
      InternalShutdown r _w -> Just r
      _ -> Nothing

    tracer :: Tracer IO Text
    tracer = trTransformer MaximalVerbosity (severityNotice trace)

-- | Windows blocking file IO calls like 'hGetChar' are not interruptable by
-- asynchronous exceptions, as used by async 'cancel' (as of base-4.12).
--
-- This wrapper works around that problem by running the blocking IO in a
-- separate thread. If the parent thread receives an async cancel then it
-- will return. Note however that in this circumstance the child thread may
-- continue and remain blocked, leading to a leak of the thread. As such this
-- is only reasonable to use a fixed number of times for the whole process.
--
wrapUninterruptableIO :: IO a -> IO a
wrapUninterruptableIO action = async action >>= wait

-- | If 'ShutdownFDs' supports internal shutdown requests,
-- return its shutdown doorbell.
getShutdownDoorbell :: ShutdownFDs -> Maybe ShutdownDoorbell
getShutdownDoorbell (InternalShutdown _l doorbell) = Just doorbell
getShutdownDoorbell _ = Nothing

-- | Given the 'ShutdownDoorbell' component of 'ShutdownFDs',
--   and an explanation of the reason, request a graceful shutdown.
triggerShutdown :: ShutdownDoorbell -> Trace IO Text -> Text -> IO ()
triggerShutdown (ShutdownDoorbell (Fd shutFd)) trace reason = do
  traceWith (trTransformer MaximalVerbosity $ severityNotice trace)
    ("Ringing the node shutdown doorbell:  " <> reason)
  IO.hClose =<< IO.fdToHandle shutFd

-- | We provide an optional cross-platform method to politely request shut down.
--
-- For the duration of 'action', we gracefully handle shutdown requests,
-- external or internal, as requested by configuration in 'NodeCLI',
-- while allocating corresponding 'ShutdownFDs', and providing them to the 'action'.
withShutdownHandling
  :: NodeCLI
  -> Trace IO Text
  -> (ShutdownFDs -> IO ())
  -> IO ()
withShutdownHandling cli trace action = do
  sfds <- decideShutdownFds cli
  withShutdownHandler sfds (shutdownFile cli) trace (action sfds)
 where
   decideShutdownFds :: NodeCLI -> IO ShutdownFDs
   decideShutdownFds NodeCLI{shutdownIPC = Just fd} =
     pure $ ExternalShutdown (ShutdownListener fd)
   decideShutdownFds NodeCLI{shutdownOnSlotSynced = MaxSlotNo{}} =
     mkInternalShutdown
   decideShutdownFds _ = pure NoShutdownFDs

   mkInternalShutdown :: IO ShutdownFDs
   mkInternalShutdown = do
     (r, w) <- IO.createPipeFd
     pure $ InternalShutdown (ShutdownListener $ Fd r) (ShutdownDoorbell $ Fd w)

-- | If configuration in 'NodeCLI' and 'ShutdownFDs' agree,
-- spawn a thread that would cause node to shutdown upon ChainDB reaching the
-- configuration-defined slot.
maybeSpawnOnSlotSyncedShutdownHandler
  :: NodeCLI
  -> ShutdownFDs
  -> Trace IO Text
  -> ResourceRegistry IO
  -> ChainDB.ChainDB IO blk
  -> IO ()
maybeSpawnOnSlotSyncedShutdownHandler cli sfds trace registry chaindb =
  case (shutdownOnSlotSynced cli, sfds) of
    (MaxSlotNo maxSlot, InternalShutdown _sl sd) -> do
      traceWith (trTransformer MaximalVerbosity $ severityNotice trace)
        ("will terminate upon reaching " <> (pack $ show maxSlot) :: Text)
      spawnSlotLimitTerminator maxSlot sd
    (MaxSlotNo{}, _) -> panic
      "internal error: slot-limited shutdown requested, but no proper ShutdownFDs passed."
    _ -> pure ()
 where
  spawnSlotLimitTerminator :: SlotNo -> ShutdownDoorbell -> IO ()
  spawnSlotLimitTerminator maxSlot sd =
    void $ onEachChange registry "slotLimitTerminator" identity Nothing
      (pointSlot <$> ChainDB.getTipPoint chaindb) $
        \case
          Origin -> pure ()
          At cur -> when (cur >= maxSlot) $
            triggerShutdown sd trace
            ("spawnSlotLimitTerminator: reached target " <> show cur)
