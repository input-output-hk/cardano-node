# Building Cardano Node with nix

The [Nix Package Manager][nix] can be installed on most Linux distributions by downloading and
running the installation script:
```
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```
and following the directions.

#### IOHK Binary Cache

To improve build speed, it is possible to set up a binary cache maintained by IOHK (**this is
optional**):
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
substituters = https://cache.nixos.org https://hydra.iohk.io
trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

Once Nix is installed, log out and then log back in then:
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
./mainnet-node-local
```
Building the node itself will take considerable disk space, perhaps more than even the current size of the blockchain, but this is temporary and can be cleaned up afterwards. To optimize space requirements and speed up the build process you can compile the node with Nix and edit the `nix.conf` file as follows:
```
substituters = https://hydra.iohk.io https://cache.nixos.org/ 

trusted-substituters = 

trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= 

max-jobs = 2 # run at most two builds at once 

cores = 0 # the builder will use all available CPU cores 

nix

```

Once the node is compiled, you should run the clean up command:
`nix-collect-garbage`

In addition, there is a sample nix config that you can use which reuses cached binaries from IOHK and results in faster builds. This involves trusting IOHK as a source for binary packages. 


[nix]: https://nixos.org/nix/
