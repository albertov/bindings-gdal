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
  haskellPackages = pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      # we need language-c 0.7 for the fix to
      # https://github.com/visq/language-c/issues/34
      # FIXME: c2hs wont build build with language-c 0.7, see
      # https://github.com/haskell/c2hs/issues/194
      # language-c = self.callHackage "language-c" "0.7.0" {};
      # c2hs = hsLib.doJailbreak super.c2hs;
    };
  };
  withGdal = gdal: haskellPackages.callPackage ./.  { inherit gdal; };

  jobs = rec {
    bindings-gdal = withGdal pkgs.gdal;
    bindings-gdal_1_11 = withGdal pkgs.gdal_1_11;
  };
in
  jobs
