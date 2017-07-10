{ fetchFromGitHub, gdal, mkDerivation, base, bytestring, c2hs, Cabal, conduit
, data-default, deepseq, exceptions, filepath, ghc-prim, hspec
, hspec-core, microlens, monad-control, mtl, process, QuickCheck
, resourcet, stdenv, temporary, text, time, transformers
, transformers-base, unix, unordered-containers, vector, file-embed
, template-haskell
}:
mkDerivation {
  pname = "bindings-gdal";
  version = "2.1.1";
  src = ./.;
  preConfigure = "export GDAL_CONFIG=${gdal}/bin/gdal-config";
  setupHaskellDepends = [ base Cabal filepath process ];
  librarySystemDepends = [ gdal c2hs ];
  libraryHaskellDepends = [
    base bytestring conduit data-default deepseq exceptions ghc-prim
    microlens monad-control mtl resourcet text time transformers
    transformers-base unordered-containers vector file-embed template-haskell
  ];
  libraryToolDepends = [ c2hs gdal ];
  testHaskellDepends = [
    base bytestring conduit data-default exceptions filepath hspec
    hspec-core microlens QuickCheck temporary text time transformers
    unix vector
  ];
  description = "Bindings to the GDAL library";
  license = stdenv.lib.licenses.bsd3;
}
