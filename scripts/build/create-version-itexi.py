# create-version-itexi.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2009--2020  Graham Percival <graham@percival-music.ca>
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

""" when being called on lilypond.org, pass it the location of the
top source dir on the command-line. """

import sys
import os
import glob


# FIXME: if the depth depends on the type of build, figure it
#        out automatically.
# just like depth in our GNUmakefiles
# these links are relative from /~graham/web/
depth = "../../"
# these links are relative from the v2.13 docs
#depth = "../../../../"


VERSION_STABLE = ""
VERSION_DEVEL = ""

try:
    topDir = sys.argv[1]
except:
    myDir = os.path.dirname(sys.argv[0])
    # use two abspaths to work around some windows python bug
    topDir = os.path.join(os.path.abspath(
        myDir)+os.sep+'..'+os.sep+'..'+os.sep)
    topDir = os.path.abspath(topDir)


# TODO: this might be useful for other scripts; can we make it available?
manuals = [os.path.splitext(x)[0] for x in list(map(os.path.basename,
                                                    glob.glob(os.path.join(topDir, 'Documentation', '*.te??'))))]
#manuals = map(lambda x: 'glossary' if x=='music-glossary' else x, manuals)
manuals.append('internals')


version_file_path = os.path.join(topDir, "VERSION")

version_contents = open(version_file_path).readlines()
major = 0
minor = 0
patch = 0
for line in version_contents:
    if (line.startswith('MAJOR_VERSION')):
        major = line[14:-1]
    if (line.startswith('MINOR_VERSION')):
        minor = line[14:-1]
    if (line.startswith('PATCH_LEVEL')):
        patch = line[12:-1]
    if (line.startswith('VERSION_STABLE')):
        VERSION_STABLE = line[15:-1]
    if (line.startswith('VERSION_DEVEL')):
        VERSION_DEVEL = line[14:-1]

VERSION = str(major)+'.'+str(minor)+'.'+str(patch)


def make_macro(name, string):
    print("@macro", name)
    print(string)
    print("@end macro")
    print("")


print("@c This file was autogenerated")
print("@c     from: VERSION")
print("@c     by:   %s" % sys.argv[0])
print("")
print("@c ************************ Version numbers ************")
print("")

make_macro("version", VERSION)
make_macro("versionStable", VERSION_STABLE)
make_macro("versionDevel", VERSION_DEVEL)

print("@c *****************************************************")
