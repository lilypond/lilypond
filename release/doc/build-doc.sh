#!/bin/sh

IMAGE="registry.gitlab.com/lilypond/lilypond/doc/ubuntu-18.04:20220116"

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
../$dir/configure

echo "Building LilyPond..."
make -j$jobs

echo ""
echo "Building documentation..."
make -j$jobs CPU_COUNT=$jobs all-doc

echo ""
echo "Creating $doc_ball..."
tar -C out-www/offline-root -cJf /$doc_ball .
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
