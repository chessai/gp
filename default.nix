{ }:

let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in rec {
  hsPkgs = pkgSet.config.hsPkgs;
  gp = hsPkgs.gp.components.exes.gp;
}
