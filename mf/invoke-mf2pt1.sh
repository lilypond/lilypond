#!/bin/sh
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2020--2022  Han-Wen Nienhuys <hanwen@lilypond.org>
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


# mf2pt1 pollutes CWD, so run it in a tmp dir.

abs_path() {
  case $1 in
    /*) echo $1;;
    *) echo $PWD/$1;;
  esac
}

set -eu
mf2pt1="$1"
src="$(abs_path $2)"
target="$3"
target_path="$(abs_path ${target})"
srcdir="$(dirname ${src})"
name="$(basename ${src} .mf)"

tmp="$(dirname ${target_path})/tmp.$(basename ${target_path})"
rm -rf $tmp
mkdir $tmp
cd $tmp

export MFINPUTS="${srcdir}:..:"
export max_print_line=1000

# While calling the `mf2pt1` script, activate the filename recorder (which
# creates a `.fls` file); we use it to set up a dependencies file.
export MPOST="mpost -recorder"

${mf2pt1} --rounding=0.0001 \
  --family=$name \
  --fullname=$name \
  --name=$name $src

printf %s "${target} : " > ${name}.pfb.dep
grep '^INPUT.*mf$' ${name}.fls | sed "s|INPUT||;s|${srcdir}/||" | tr -d '\n' >> ${name}.pfb.dep

mv *.pfb *.tfm *.log *.dep ..
cd ..
rm -rf ${tmp}
