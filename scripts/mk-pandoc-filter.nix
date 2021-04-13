{ runCommand, ghcWithPackages, filterHsFile, filterName }:

runCommand "mk-pandoc-filter" {} ''
   mkdir -p $out/bin
   ${ghcWithPackages (pkgs: with pkgs; [ pandoc ])}/bin/ghc --make ${filterHsFile} -o $out/bin/${filterName}
''
