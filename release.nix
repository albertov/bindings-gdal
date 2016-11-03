let
  pkgs = import <nixpkgs> {};
  withGdal = gdal:
    let
      ghc = pkgs.haskell.packages.ghc801;
      g = ghc.override {
            overrides = self: super:
              { bindings-gdal = self.callPackage ./.  { inherit gdal; }; };
          };
    in g.ghcWithPackages (ps : [ ps.bindings-gdal ]);

  jobs = rec {
    bindings-gdal = withGdal pkgs.gdal;
    bindings-gdal_1_11 = withGdal pkgs.gdal_1_11;
  };
in
  jobs
