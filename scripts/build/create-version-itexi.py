#!@PYTHON@
# create-version-itexi.py

""" when being called on lilypond.org, pass it the location of the
top source dir on the command-line. """

import sys
import os
import glob



# FIXME: if the depth depends on the type of build, figure it
#        out automatically.
### just like depth in our GNUmakefiles
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
    topDir = os.path.join(os.path.abspath(myDir)+os.sep+'..'+os.sep+'..'+os.sep)
    topDir = os.path.abspath( topDir )


# TODO: this might be useful for other scripts; can we make it available?
manuals = map(lambda x: os.path.splitext(x)[0],
              map(os.path.basename,
                  glob.glob(os.path.join(topDir,'Documentation', '*.te??'))))
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
    print "@macro", name
    print string
    print "@end macro"
    print ""

print "@c ************************ Version numbers ************"
make_macro("version", VERSION)
make_macro("versionStable", VERSION_STABLE)
make_macro("versionDevel", VERSION_DEVEL)


