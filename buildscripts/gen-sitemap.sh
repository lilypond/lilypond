#!/bin/sh

TMPDIR=/tmp/gen-sitemap
for tarball in $*; do
    tar -C $TMPDIR -xfz $tarball '*.html'
done
