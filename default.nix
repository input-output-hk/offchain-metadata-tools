# Compatibility shim for non-flake use (`nix-build`, and the NixOS
# module defaults). The flake is the single source of truth.
{ system ? builtins.currentSystem or "x86_64-linux" }:
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flakeCompatNode = lock.nodes.flake-compat.locked;
  flake-compat = fetchTarball {
    url = "https://github.com/${flakeCompatNode.owner}/${flakeCompatNode.repo}/archive/${flakeCompatNode.rev}.tar.gz";
    sha256 = flakeCompatNode.narHash;
  };
  self = (import flake-compat { src = ./.; }).defaultNix;
in
{
  inherit (self) hydraJobs nixosModules;
  project = self.project.${system};
  packages = self.packages.${system};
  devShells = self.devShells.${system};
  pkgs = self.legacyPackages.${system};
}
