{ stdenv, writeScript, coreutils, nixStable, git, gawk }:

with stdenv.lib;

writeScript "check-lint-fuzz.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ stdenv.shellPackage coreutils nixStable git gawk ]}:$PATH"

  cd $(git rev-parse --show-toplevel)

  git ls-files | awk '/.*hs$/ { print "Checking " $0; }'
''
