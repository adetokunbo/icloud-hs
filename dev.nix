{
  # Pin haskell-nix to a recent (as of 2025/06/21) commit
  h8x-commit = "a3f435537829d97a1d2b1d27675963ac3ab21151";

  # Pin hackage-nix as of 2026-07-21
  hackage-nix-commit = "6b96f66aedcdb1edb2be0436ea759d369d61ec97";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc98";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2026-07-21T00:00:00Z";
}
