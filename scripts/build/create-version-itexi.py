#!@PYTHON@
# create-version-itexi.py

import sys
import os

#print "create-version-itexi.py"

VERSION_STABLE = ""
VERSION_DEVEL = ""

myDir = os.path.dirname(sys.argv[0])
# use two abspaths to work around some windows python bug
topDir = os.path.join(os.path.abspath(myDir)+os.sep+'..'+os.sep+'..'+os.sep)
topDir = os.path.abspath( topDir )

version_file_path = os.path.join(topDir, "VERSION")

version_contents = open(version_file_path).readlines()
for line in version_contents:
	if (line[0:14] == 'VERSION_STABLE'):
		VERSION_STABLE = line[15:-1]
	if (line[0:13] == 'VERSION_DEVEL'):
		VERSION_DEVEL = line[14:-1]

def make_macro(name, string):
	print "@macro", name
	print string
	print "@end macro"
	print ""

def make_download(name, osA, osB, version, revision, text):
	string = "@uref{http://download.linuxaudio.org/lilypond/binaries/"
	string += osA + "lilypond-"
	string += version + "-" + revision
	string += "." + osB + ",\n"
	string += text
	string += ": LilyPond "
	string += version + "-" + revision
	string += "}"
	make_macro(name, string)

def make_all_downloads(macroName, version):
	make_download("download"+macroName+"LinuxNormal", "linux-x86/",
		"linux-x86.sh",	version, "1", "Linux x86")
	make_download("download"+macroName+"LinuxBig", "linux-64/",
		"linux-64.sh", version, "1", "Linux 64")
	make_download("download"+macroName+"LinuxPPC", "linux-ppc/",
		"linux-ppc.sh", version, "1", "Linux PPC")
	
	make_download("download"+macroName+"FreeBSDNormal", "freebsd-x86/",
		"freebsd-x86.sh", version, "1", "FreeBSD i386")
	make_download("download"+macroName+"FreeBSDBig", "freebsd-x86/",
		"freebsd-64.sh", version, "1", "FreeBSD amd64")
	
	make_download("download"+macroName+"DarwinNormal", "darwin-x86/",
		"darwin-x86.tar.bz2", version, "1", "MacOS X x86")
	make_download("download"+macroName+"DarwinPPC", "darwin-ppc/",
		"darwin-ppc.tar.bz2", version, "1", "MacOS X PPC")

	make_download("download"+macroName+"Windows", "mingw/",
		"mingw.exe", version, "1", "Windows")


make_macro("versionStable", VERSION_STABLE)
make_macro("versionDevel", VERSION_DEVEL)

make_all_downloads("Stable", VERSION_STABLE)
make_all_downloads("Devel", VERSION_DEVEL)

