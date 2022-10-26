#!/bin/sh

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2022--2022 Jonas Hahnfeld <hahnjo@hahnjo.de>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

IMAGE="registry.gitlab.com/lilypond/lilypond/doc/ubuntu-18.04:20221027"

if [ $# -lt 1 ]; then
  echo "Too few arguments, pass the archive with LilyPond's source code" >&2
  exit 1
fi
path="$1"
archive="$(basename $path)"
dir="$(basename $archive .tar.gz)"

doc_ball="$dir-documentation.tar.xz"
web_ball="$dir-webdoc.tar.xz"

jobs="$(nproc)"
if [ $# -gt 1 ]; then
  jobs="$2"
fi

# Start with --tty to prevent the container from stopping.
container=$(docker run --detach --tty "$IMAGE")
if [ $? -ne 0 ]; then
  echo "Failed to start container" >&2
  exit 1
fi
echo "Successfully started container $container"

docker cp "$path" "$container:/$archive"
if [ $? -ne 0 ]; then
  echo "Failed to copy the archive" >&2
  exit 1
fi

# Create a build script in the container. We do this so that the following
# docker-exec can pass --interactive and --tty and the user is able to send
# CTRL-C and other input to the container.
docker exec --interactive "$container" sh -c "cat > build.sh" <<EOF
# Fail on errors.
set -e

echo "Extracting $archive..."
cd /
tar -xf $archive

echo "Configuring build..."
mkdir build
cd build
../$dir/configure --prefix=/

echo "Building LilyPond..."
make -j$jobs
# Also byte-compile the Scheme files to get proper argument names in the IR.
make bytecode

echo ""
echo "Building documentation..."
make -j$jobs CPU_COUNT=$jobs all-doc

echo ""
echo "Installing documentation for $doc_ball..."
make install-doc install-help2man DESTDIR=/install

echo ""
echo "Creating $doc_ball..."
tar -C /install -cJf /$doc_ball .
echo "Creating $web_ball..."
tar -C out-www/online-root -cJf /$web_ball .
EOF
if [ $? -ne 0 ]; then
  echo "Failed to create build script" >&2
  exit 1
fi

docker exec --interactive --tty "$container" sh build.sh
if [ $? -ne 0 ]; then
  echo "Failed to build the documentation" >&2
  exit 1
fi

docker cp "$container:/$doc_ball" .
if [ $? -ne 0 ]; then
  echo "Failed to copy $doc_ball" >&2
  exit 1
fi

docker cp "$container:/$web_ball" .
if [ $? -ne 0 ]; then
  echo "Failed to copy $web_ball" >&2
  exit 1
fi

# If we got this far without failing, delete the container.
# (--force so that we don't have to stop it first)
docker rm --force "$container" >/dev/null

exit 0
