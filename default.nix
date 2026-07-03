let
  dev = import ./dev.nix;
  pkgs-nix = import ./h8x.nix;
in
{ pkgs ? pkgs-nix
, ghc ? dev.compiler-nix-name
} : pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "icloud-auth";
    src = ./.;
  };
  compiler-nix-name = ghc;
  inherit (dev) index-state;
}
