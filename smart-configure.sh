#!/bin/sh

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2007--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
#                           Jan Nieuwenhuizen <janneke@gnu.org>
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

srcdir=${srcdir:-.}
set -ux

MAKEFILE_MD5=`find $srcdir -name GNUmakefile | grep -v '^./GNUmakefile$' | sort | md5sum | cut -b 1-32`
CONFIGURE_INPUT_MD5=`cat $srcdir/config.make.in $srcdir/config.hh.in $srcdir/GNUmakefile.in | md5sum | cut -b 1-32`
CONFIGURE_OPTIONS_MD5=`echo "$@" | tr ' ' '\n' | sed 's/  */ /g' | grep '.' | sort -u | md5sum | cut -b 1-32`

CONFIGURE_CHECKSUM_FILE=configure.checksum
CONFIGURE_CHECKSUM="$MAKEFILE_MD5$CONFIGURE_INPUT_MD5$CONFIGURE_OPTIONS_MD5"

if test `cat $CONFIGURE_CHECKSUM_FILE` = "$CONFIGURE_CHECKSUM" ; then
  exit 0
fi

( set +ux; echo Invoking configure...; $srcdir/configure "$@" ) || exit 1
printf "%s" $CONFIGURE_CHECKSUM > $CONFIGURE_CHECKSUM_FILE

