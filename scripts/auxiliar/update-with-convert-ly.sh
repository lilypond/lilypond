#!/usr/bin/env bash

### are we in the top source dir?
if [ ! -e scripts/convert-ly.py ]; then
  echo "Must run from top source directory"
  exit 1
fi

### get the directories
TOP_SRC_DIR=`pwd`
if [ -z $BUILD_DIR ]; then
  BUILD_DIR=.
fi

### make sure convert-ly is up-to-date
touch python/convertrules.py
touch scripts/convert-ly.py
cd $BUILD_DIR
make
cd $TOP_SRC_DIR

### update manuals
find Documentation/ -path 'Documentation/snippets' -prune \
  -o -name '*.itely' | xargs $BUILD_DIR/out/bin/convert-ly -e -d

### update .ly files
find . -name '*.ly' | xargs $BUILD_DIR/out/bin/convert-ly -e -d
