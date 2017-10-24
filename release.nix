let
  revision = "36a4dc392a40e06973a64b7666b179f4a021682a";
  nixpkgsUrl =
    "https://github.com/NixOS/nixpkgs-channels/archive/${revision}.tar.gz";
  pkgs = import (fetchTarball nixpkgsUrl) {
     config = {
       packageOverrides = super: {
         # Needed for language-c 0.7. Remove once nixpkgs includes it
         all-cabal-hashes = super.fetchFromGitHub {
           owner = "commercialhaskell";
           repo = "all-cabal-hashes";
           rev = "1ece7c6737619c9720f0c8427e9a2d12a7b316e5";
           sha256 = "0qbjm81ql4g3cadly15mp0xljrbnymp26n44312krywbanvjdxdl";
         };
       };
     };
  };
  hsLib = import "${pkgs.path}/pkgs/development/haskell-modules/lib.nix"
            { inherit pkgs; inherit (pkgs) lib;};
  # Need this branch of c2hs until a release supports language-c 0.7, see
  # https://github.com/haskell/c2hs/issues/194
  c2hsSrc = pkgs.fetchFromGitHub {
    owner = "deech";
    repo = "c2hs";
    rev = "8b79823c32e234c161baec67fdf7907952ca62b8";
    sha256 = "0hyrcyssclkdfcw2kgcark8jl869snwnbrhr9k0a9sbpk72wp7nz";
  };
  haskellPackages = pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      # we need language-c 0.7 for the fix to
      # https://github.com/visq/language-c/issues/34
      language-c = self.callHackage "language-c" "0.7.0" {};
      c2hs = hsLib.dontCheck (super.callCabal2nix "c2hs" c2hsSrc {});
    };
  };
  withGdal = gdal: haskellPackages.callPackage ./.  { inherit gdal; };

  jobs = rec {
    bindings-gdal = withGdal pkgs.gdal;
    bindings-gdal_1_11 = withGdal pkgs.gdal_1_11;
  };
in
  jobs
