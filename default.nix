let pkgs = import ./nixpkgs.nix;

in pkgs.haskellPackages.callCabal2nix "haskell-verified-examples" ./. { }
