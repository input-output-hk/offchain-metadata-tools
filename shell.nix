# Compatibility shim for `nix-shell`; prefer `nix develop`.
{ system ? builtins.currentSystem
, profiling ? false
}:
let
  self = import ./default.nix { inherit system; };
in
if profiling
then self.devShells.profiled
else self.devShells.default
