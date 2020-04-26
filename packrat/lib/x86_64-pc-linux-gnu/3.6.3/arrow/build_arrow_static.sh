#!/bin/bash
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

# Quit on failure
# set -e

# Print commands for debugging
set -x

# By default, this script assumes it's in the top-level dir of the apache/arrow
# git repository. Set any of the following env vars to customize where to read
# and write from
: ${ARROW_HOME:="$(pwd)"}                       # Only used in default SOURCE/BUILD dirs
: ${SOURCE_DIR:="${ARROW_HOME}/cpp"}            # Where the C++ source is
: ${BUILD_DIR:="${ARROW_HOME}/r/libarrow/dist"} # Where cmake should build
: ${DEST_DIR:="$BUILD_DIR"}                     # Where the resulting /lib and /include should be
: ${CMAKE:="$(which cmake)"}

# Make sure SOURCE and DEST dirs are absolute and exist
SOURCE_DIR="$(cd "${SOURCE_DIR}" && pwd)"
DEST_DIR="$(mkdir -p "${DEST_DIR}" && cd "${DEST_DIR}" && pwd)"

if [ "$CMAKE_GENERATOR" = "" ]; then
  # Look for ninja, prefer it
  if which ninja >/dev/null 2>&1; then
    CMAKE_GENERATOR="Ninja"
  fi
fi

if [ "$LIBARROW_MINIMAL" = "false" ]; then
  ARROW_JEMALLOC=ON
  ARROW_WITH_BROTLI=ON
  ARROW_WITH_BZ2=ON
  ARROW_WITH_LZ4=ON
  ARROW_WITH_SNAPPY=ON
  ARROW_WITH_ZLIB=ON
  ARROW_WITH_ZSTD=ON
fi

mkdir -p "${BUILD_DIR}"
pushd "${BUILD_DIR}"
${CMAKE} -DARROW_BOOST_USE_SHARED=OFF \
    -DARROW_BUILD_TESTS=OFF \
    -DARROW_BUILD_SHARED=OFF \
    -DARROW_BUILD_STATIC=ON \
    -DARROW_COMPUTE=ON \
    -DARROW_CSV=ON \
    -DARROW_DATASET=ON \
    -DARROW_DEPENDENCY_SOURCE=${ARROW_DEPENDENCY_SOURCE:-AUTO} \
    -DARROW_FILESYSTEM=ON \
    -DARROW_JEMALLOC=${ARROW_JEMALLOC:-ON} \
    -DARROW_JSON=ON \
    -DARROW_PARQUET=ON \
    -DARROW_WITH_BROTLI=${ARROW_WITH_BROTLI:-OFF} \
    -DARROW_WITH_BZ2=${ARROW_WITH_BZ2:-OFF} \
    -DARROW_WITH_LZ4=${ARROW_WITH_LZ4:-OFF} \
    -DARROW_WITH_SNAPPY=${ARROW_WITH_SNAPPY:-OFF} \
    -DARROW_WITH_ZLIB=${ARROW_WITH_ZLIB:-OFF} \
    -DARROW_WITH_ZSTD=${ARROW_WITH_ZSTD:-OFF} \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DCMAKE_INSTALL_PREFIX=${DEST_DIR} \
    -DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=ON \
    -DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON \
    -DCMAKE_UNITY_BUILD=ON \
    -DOPENSSL_USE_STATIC_LIBS=ON \
    ${EXTRA_CMAKE_FLAGS} \
    -G ${CMAKE_GENERATOR:-"Unix Makefiles"} \
    ${SOURCE_DIR}
${CMAKE} --build . --target install

if [ $? -ne 0 ] && [ "${DEBUG_DIR}" != "" ]; then
  # For debugging installation problems, copy the build contents somewhere not tmp
  mkdir -p ${DEBUG_DIR}
  cp -r ./* ${DEBUG_DIR}
fi

# Copy the bundled static libs from the build to the install dir
# See https://issues.apache.org/jira/browse/ARROW-7499 for moving this to CMake
find . -regex .*/.*/lib/.*\\.a\$ | xargs -I{} cp -u {} ${DEST_DIR}/lib
# jemalloc makes both libjemalloc.a and libjemalloc_pic.a; we can't use the former, only the latter
rm ${DEST_DIR}/lib/libjemalloc.a || true
# -lbrotlicommon-static needs to come after the other brotli libs, so rename it so alpha sort works
if [ -f "${DEST_DIR}/lib/libbrotlicommon-static.a" ]; then
  mv "${DEST_DIR}/lib/libbrotlicommon-static.a" "${DEST_DIR}/lib/libbrotlizzz-static.a"
fi
popd
