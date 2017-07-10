let
  revision = "81fceb255448415e70b9e7775d590b6def45f861";
  nixpkgsUrl =
    "https://github.com/NixOS/nixpkgs-channels/archive/${revision}.tar.gz";
  pkgs = import (fetchTarball nixpkgsUrl) {};
  withGdal = gdal:
    let
      packages = pkgs.haskell.packages.ghc802;
    in packages.callPackage ./.  { inherit gdal; };

  jobs = rec {
    bindings-gdal = withGdal pkgs.gdal;
    bindings-gdal_1_11 = withGdal pkgs.gdal_1_11;
  };
in
  jobs
