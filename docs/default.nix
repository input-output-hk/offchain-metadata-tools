{ emacs, git, runCommand, lib, coreutils, gawk, curl, jq }:

let
  buildInputs =
    [ emacs
      git
      coreutils
      gawk
      curl
    ];
in
  runCommand
    "generate-documentation-scripts"
    { inherit buildInputs; }
    ''
      mkdir -p $out/bin
      cd $out/bin

      cp ${./index.org} ./index.org
      emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "./index.org")'
      mv ./index.org $out/index.org
      bash ./mk-nix-setup.sh
      mv metadata-config.nix $out/
    ''
