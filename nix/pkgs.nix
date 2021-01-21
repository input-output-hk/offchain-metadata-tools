# our packages overlay
pkgs: _: with pkgs; {
  metadataServerHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
