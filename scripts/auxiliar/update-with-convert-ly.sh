#!/usr/bin/env bash
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2010--2022 Graham Percival <graham@percival-music.ca>
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


### are we in the top source dir?
if [ ! -e scripts/convert-ly.py ]; then
  echo "Must run from top source directory"
  exit 1
fi

### get the build directory
if [ -z $LILYPOND_BUILD_DIR ]; then
  LILYPOND_BUILD_DIR=.
fi

### update manuals
find Documentation/ -path 'Documentation/snippets' -prune -o -name out -prune \
  -o -name 'out-*' -prune -o -name '*.itely' -print \
  | xargs $LILYPOND_BUILD_DIR/out/bin/convert-ly -e -d "$@"

### update .ly files
# don't look in . otherwise it'll find stuff in build/ !
find Documentation/ input/ ly/ -name out -prune -o -name 'out-*' -prune \
  -o \( -name '*.ly' -o -name '*.ily' \) -print \
  | xargs $LILYPOND_BUILD_DIR/out/bin/convert-ly -e -d "$@"
