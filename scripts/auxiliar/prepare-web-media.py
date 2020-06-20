#!/usr/bin/env python
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2011--2020 Graham Percival <graham@percival-music.ca>
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


import os
import sys
import glob
import shutil

try:
    lilypond_git_dir = os.environ["LILYPOND_GIT"]
    lilypond_web_media_dir = os.environ["LILYPOND_WEB_MEDIA_GIT"]
except KeyError:
    print("Error: you must have these environment variables defined:")
    print("  $LILYPOND_GIT")
    print("  $LILYPOND_WEB_MEDIA_GIT")
    sys.exit(1)

build_dir = os.path.join(lilypond_git_dir, 'build')

def get_pictures_from(dirname):
    try:
        examine_dirname = os.path.join(build_dir,
            "Documentation", dirname, "out-www")
        filenames = (
            glob.glob(os.path.join(examine_dirname, "*.png")) +
            glob.glob(os.path.join(examine_dirname, "*.jpg")))
    except:
        print("Cannot find files (maybe a problem with your build directory?)")
    return filenames

pictures_filenames = get_pictures_from("pictures")
ly_examples_filenames = get_pictures_from(os.path.join("web", "ly-examples"))

pictures_dest = os.path.join(lilypond_web_media_dir, "pictures")
ly_examples_dest = os.path.join(lilypond_web_media_dir, "ly-examples")
for filename in pictures_filenames:
    shutil.copy(filename, pictures_dest)
for filename in ly_examples_filenames:
    shutil.copy(filename, ly_examples_dest)

print("Finished copying.")
print("Don't forget to git commit and push to the lilypond-web-media repository!")

