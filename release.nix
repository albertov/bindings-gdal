let
  revision = "36a4dc392a40e06973a64b7666b179f4a021682a";
  nixpkgsUrl =
    "https://github.com/NixOS/nixpkgs-channels/archive/${revision}.tar.gz";
  pkgs = import (fetchTarball nixpkgsUrl) {};

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

  # we need language-c 0.7 for the fix to
  # https://github.com/visq/language-c/issues/34.
  # From github until the hash from the hackage release
  # lands in nixpkgs all-cabal-hashes
  languageCsrc = pkgs.fetchFromGitHub {
    owner = "visq";
    repo = "language-c";
    rev = "54dfce30add1f0ac1d55413ecf59e5f08a49258d";
    sha256 = "0ilfd73336745k7f58vmvrm5j06z977g7pjrl9pdx6fbx3m3aqyb";
  };

  haskellPackages = pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      language-c = self.callCabal2nix "language-c" languageCsrc {};
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
