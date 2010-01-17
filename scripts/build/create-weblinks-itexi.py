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

def make_download_source(name, vstring, version):
    string = "@uref{http://download.linuxaudio.org/lilypond/sources/"
    string += vstring + "/"
    string += "lilypond-" + version + ".tar.gz"
    string += ", "
    string += "Source: lilypond-" + version + ".tar.gz"
    string += "}"
    make_macro(name, string)

def make_all_downloads(macroName, version):
    make_download("download"+macroName+"LinuxNormal", "linux-x86/",
        "linux-x86.sh",    version, "1", "Linux x86")
    make_download("download"+macroName+"LinuxBig", "linux-64/",
        "linux-64.sh", version, "1", "Linux 64")
    make_download("download"+macroName+"LinuxPPC", "linux-ppc/",
        "linux-ppc.sh", version, "1", "Linux PPC")

    make_download("download"+macroName+"FreeBSDNormal", "freebsd-x86/",
        "freebsd-x86.sh", version, "1", "FreeBSD i386")
    make_download("download"+macroName+"FreeBSDBig", "freebsd-64/",
        "freebsd-64.sh", version, "1", "FreeBSD amd64")

    make_download("download"+macroName+"DarwinNormal", "darwin-x86/",
        "darwin-x86.tar.bz2", version, "1", "MacOS X x86")
    make_download("download"+macroName+"DarwinPPC", "darwin-ppc/",
        "darwin-ppc.tar.bz2", version, "1", "MacOS X PPC")

    make_download("download"+macroName+"Windows", "mingw/",
        "mingw.exe", version, "1", "Windows")

def make_ver_link(macroname, url, linktext):
    string = "@uref{"
    string += url
    string += ","
    string += linktext
    string += "}"
    make_macro(macroname, string)

# TODO: this kind of thing should really be in a central place for
# lilypond python build scripts
def translateNameToUrl(manual, version):
    ver_split = version.split('.')
    ver_minor = ver_split[0] + '.' + ver_split[1]
    url = depth + "doc/v" + ver_minor + "/Documentation/"

    if (ver_minor == '2.13'):
        return url+manual
    if (ver_minor == '2.12'):
        if (manual=='learning'):
            return url+'user/lilypond-learning'
        elif (manual=='internals'):
            return url+'user/lilypond-internals'
        elif (manual=='notation'):
            return url+'user/lilypond'
        elif (manual=='usage'):
            return url+'user/lilypond-program'
        elif (manual=='snippets'):
            return url+'../input/lsr/lilypond-snippets'
        elif (manual=='changes'):
            return url+'topdocs/NEWS.html'
        elif (manual=='music-glossary'):
            return url+'user/music-glossary'
        elif (manual=='essay'):
            return url+'user/lilypond-learning/Background.html'
        elif (manual=='extending'):
            return url+'user/lilypond/Interfaces-for-programmers.html'
        else:
            return ''


def make_manual_links(name, version):
    for m in manuals:
        manual = m
        if (m=='music-glossary'):
            mshort = 'Glossary'
        else:
            mshort = m.capitalize()
        url = translateNameToUrl(m, version)

        if (url == ''):
            # can't have a comma here due to texinfo
            make_ver_link("manual"+name+mshort+'Pdf',
                "http://lilypond.org",
                mshort+" (did not exist in 2.12)")
            make_ver_link("manual"+name+mshort+'Split',
                "http://lilypond.org",
                mshort+" (did not exist in 2.12)")
            make_ver_link("manual"+name+mshort+'Big',
                "http://lilypond.org",
                mshort+" (did not exist in 2.12)")
            make_ver_link("manual"+name+mshort+'SplitNoName',
                "http://lilypond.org",
                mshort+" (did not exist in 2.12)")
            continue
        make_ver_link("manual"+name+mshort+'Pdf',
                  url + '.pdf',
                  manual.capitalize() + '.pdf')
        make_ver_link("manual"+name+mshort+'Split',
                  url + '/index.html',
                  manual.capitalize() + ' (split HTML)')
        make_ver_link("manual"+name+mshort+'Big',
                  url + '-big-page.html',
                  manual.capitalize() + ' (big HTML)')
	# this is stupid and I shouldn't have bothered trying
	# to support the 2.12 docs and it will be deleted once
	# 2.14 is out and the website won't be visible to users
        # until 2.14 is out.  -gp
        if (url.endswith('.html')):
            newurl = url
        else:
            newurl = url + '/index.html'
        make_ver_link("manual"+name+mshort+'SplitNoName',
                  newurl,
                  manual.capitalize())

def make_regtest_links(name, version):
    ver_split = version.split('.')
    ver_minor = ver_split[0] + '.' + ver_split[1]
    url = depth + "doc/v" + ver_minor + "/input/regression/"

    make_ver_link("regtest"+name, url+"collated-files.html",
        "Regression tests for "+version)
    make_ver_link("regtest"+name+"Pdf", url+"collated-files.pdf",
        "pdf of "+version+" regtests")
    make_ver_link("regtest"+name+"Xml", url+"musicxml/collated-files.html",
        "MusicXML Regression tests for "+version)
    make_ver_link("regtest"+name+"XmlPdf", url+"musicxml/collated-files.html",
        "pdf of "+version+" musicxml regtests")

def make_doctarball_links(name, version):
    url = depth + "download/binaries/documentation/lilypond-"
    url += version
    url += ".documentation.tar.bz2"
    make_ver_link("doctarball"+name, url, "Doc tarball for "+version)

print "@c ************************ Download binaries ************"
make_all_downloads("Stable", VERSION_STABLE)
make_all_downloads("Devel", VERSION_DEVEL)

print "@c ************************ Download source ************"
# FIXME: icky hard-coding!  -gp
make_download_source("downloadStableSource", "v2.12", VERSION_STABLE)
make_download_source("downloadDevelSource", "v2.13", VERSION_DEVEL)

print "@c ************************ Manual links ************"
make_manual_links("Stable", VERSION_STABLE)
make_manual_links("Devel", VERSION_DEVEL)
make_doctarball_links("Stable", VERSION_STABLE)
make_doctarball_links("Devel", VERSION_STABLE)

print "@c ************************ Regtest links ************"
make_regtest_links("Stable", VERSION_STABLE)
make_regtest_links("Devel", VERSION_DEVEL)

