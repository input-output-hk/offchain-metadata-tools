{ stdenv, lib, writeScript, coreutils, glibc, git, openssh, gnused, mkdocs,
  runCommand, ghcWithPackages, pandoc }:

with lib;

let
  repo = "git@github.com:input-output-hk/offchain-metadata-tools.git";
  sshKey = "/run/keys/buildkite-haskell-dot-nix-ssh-private";

  filterName = "orgMdFilter";
  pandocOrgMdFilter = import ./mk-pandoc-filter.nix {
    inherit runCommand ghcWithPackages filterName;
    filterHsFile = ./pandoc-filters/org-to-md.hs;
  };
in
  # update-docs depends on glibc which doesn't build on darwin
  meta.addMetaAttrs { platforms = platforms.linux; } (writeScript "update-docs.sh" ''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH="${makeBinPath [ coreutils glibc git openssh gnused mkdocs pandocOrgMdFilter pandoc ]}"

    source ${./git.env}

    rev=$(git rev-parse --short HEAD)
    cd $(git rev-parse --show-toplevel)

    echo "Preprocessing..."
    echo "   Removing org-mode peculiarities..."
    pandoc --filter ${filterName} docs/index.org -o docs/index.md

    echo "Building..."
    rm -rf site
    mkdocs build
    touch site/.nojekyll
    sed -i -e '/Build Date/d' site/index.html
    sed -i -e '/lastmod/d' site/sitemap.xml
    rm -f site/sitemap.xml.gz

    echo "Updating git index..."
    git fetch origin
    git checkout gh-pages
    git reset --hard origin/gh-pages
    GIT_WORK_TREE=$(pwd)/site git add -A
    check_staged
    echo "Committing changes..."
    git commit --no-gpg-sign --message "Update gh-pages for $rev"

    if [ "''${BUILDKITE_BRANCH:-}" = master ]; then
      git push ${repo} HEAD:gh-pages
    fi
  '')
