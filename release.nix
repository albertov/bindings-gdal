let
  revision = "36a4dc392a40e06973a64b7666b179f4a021682a";
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
