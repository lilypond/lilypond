#!/bin/sh
#
# Run this to generate configure and initial GNUmakefiles

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2002--2022  Jan Nieuwenhuizen <janneke@gnu.org>,
#                           Han-Wen Nienhuys <hanwen@xs4all.nl>
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

srcdir=`dirname -- $0`

# Canonicalize paths.
srcdir=`cd "$srcdir"; pwd`
builddir=`pwd`

for arg; do
    case $arg in
        --ci)
            CI=true
            shift
            ;;

        --currdir)
            CURRDIR=true
            shift
            ;;

        --help)
            exec echo "\
Usage: $0 [OPTION]... [CONFIGURE-OPTION]...
Generate LilyPond's 'configure' script, then execute it
with CONFIGURE-OPTION arguments.

  --ci           also set 'configure' flags for gitlab's CI
  --currdir      generate 'configure' script in current directory
                   instead of the location of 'autogen.sh'
  --help         display this help and exit
  --noconfigure  don't execute generated 'configure' script"
            exit 0
            ;;

        --noconf*)
            NOCONFIGURE=true
            shift
            ;;

        *)
            break
            ;;
    esac
done

if test ! -w "$srcdir"; then
    echo "Directory '$srcdir' is write-only, assuming option '--currdir'"
    CURRDIR=true
fi

if test -n "$CURRDIR" && test ! -w "$builddir"; then
    echo >&2 "$0: Directory '$builddir' is write-only, can't proceed"
    exit 1
fi

parent=`dirname "$builddir"`
if test "$srcdir" = "$builddir"; then
    prefix=.
elif test "$srcdir" = "$parent"; then
    prefix=..
else
    prefix="$srcdir"
fi

if test -n "$CURRDIR"; then
    configure=./configure
    if test "$prefix" != . && test "$prefix" != ..; then
        suffix=" --srcdir $srcdir"
    fi

    echo "Running autoconf in '$builddir' ..."
    SRCDIR="$srcdir" \
          autoconf -I "$srcdir" -o configure "$srcdir/configure.ac" \
        || exit 1
else
    configure="$prefix/configure"
    (
        cd "$srcdir"
        echo "Running autoconf in '$srcdir' ..."
        autoconf || exit 1
    )
fi

if test -n "$NOCONFIGURE"; then
    echo "You can now run '$configure$suffix [...]'"
    exit 0
fi

if test -n "$CI" ; then
    conf_flags="$conf_flags --enable-checking"
    conf_flags="$conf_flags --enable-gs-api"
    conf_flags="$conf_flags --disable-debugging"

    CFLAGS="$CFLAGS -Werror"
    export CFLAGS
fi

if test -z "${conf_flags}$*"; then
    cat <<EOF
    Warning: about to run the 'configure' script without arguments.
    Add arguments to the '$0' command line
    to pass them to 'configure'.

    Invoke with '--noconfigure' to skip execution of 'configure'.
EOF
fi

echo Running $configure$suffix $conf_flags "$@" ...
exec "$configure" $suffix $conf_flags "$@"
