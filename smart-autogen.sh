#!/bin/sh

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2007--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

AUTOGEN_INPUT_CHECKSUM=`cat $srcdir/configure.ac $srcdir/aclocal.m4 | md5sum | cut -b 1-32`

CHECKSUM_FILE=autogen.checksum

if test `cat $CHECKSUM_FILE`"" = "$AUTOGEN_INPUT_CHECKSUM"; then
  exit 0
fi

( set +ux; echo Invoking autogen.sh...; ${srcdir}/autogen.sh "$@" ) || exit 1
printf "%s" $AUTOGEN_INPUT_CHECKSUM > $CHECKSUM_FILE
