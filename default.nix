{ }:

let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/13f7dbe3f330e72c9af48845bb680d50fbd7e055.tar.gz) { };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

  hsPkgs = pkgSet.config.hsPkgs;

in hsPkgs.gp.components.exes.gp
