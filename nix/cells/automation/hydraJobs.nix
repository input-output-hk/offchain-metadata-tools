{
  cell,
  inputs,
}:

import "${inputs.self}/release.nix" {
  metadata-server = inputs.self;
  supportedSystems = [inputs.nixpkgs.system];
}
