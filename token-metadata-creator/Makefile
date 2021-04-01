
dev:
	nix-shell --run "ghcid -c 'cabal repl $(target) --project-file=cabal-nix.project'"

repl:
	nix-shell --run "cabal repl $(target) --project-file=cabal-nix.project"

style: ## Apply stylish-haskell on all *.hs files
	nix-shell --pure --run 'find . -type f -name "*.hs" -not -path ".git" -not -path "*.stack-work*" -not -path "./dist*" -print0 | xargs -0 stylish-haskell -i'
