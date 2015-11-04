echo "Installing GDAL"

export GDAL="2.0.1"

export GDAL_URL=http://download.osgeo.org/gdal/${GDAL}/gdal-${GDAL}.tar.gz
export PATH=$HOME/.local/bin:$PATH
export GDAL_PREFIX=$HOME/.local/gdal-$GDAL
export GDAL_BIN=$GDAL_PREFIX/bin
export GDAL_LIB=$GDAL_PREFIX/lib
export GDAL_SRC=/tmp/gdal-$GDAL-src

if [ ! -f $GDAL_BIN/gdal-config ]; then
  mkdir -p $GDAL_PREFIX
  mkdir -p $GDAL_SRC
  curl -L $GDAL_URL | tar xz --strip-components=1 -C $GDAL_SRC
  pushd $GDAL_SRC
  ./configure --prefix=$GDAL_PREFIX \
              --enable-debug \
              --without-libtool \
              --without-php \
              --without-python \
              --without-java \
              --without-perl \
              --without-ruby \
              --without-odbc
  make -j3
  make install
  popd
fi

export PATH=$GDAL_BIN:$PATH
# It seems stack doesn't use the extraLibs and extraLibDirs that we set in
# Setup.hs so the test suite can't link to the library unless we set
# LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$GDAL_LIB:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=$GDAL_LIB:$DYLD_LIBRARY_PATH
