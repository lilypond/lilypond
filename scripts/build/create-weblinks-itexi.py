#!@PYTHON@
# -*- coding: utf-8 -*-
# create-version-itexi.py

""" when being called on lilypond.org, pass it the location of the
top source dir on the command-line. """

import sys
import os
import glob

# these links are relative from /website/ on lilypond.org
depth = "../"

### translation data -- shouldn't be here; see issue
### http://code.google.com/p/lilypond/issues/detail?id=1050

langs = ['', 'de', 'es', 'fr', 'hu', 'it', 'ja', 'nl']

# Get/update node translations
'''
for i in de es fr hu it ja nl; do
    echo "'"$i"': {"
    (echo '--' ; grep -nH -B1 translationof Documentation/$i/web/* ) \
        | pytt '^--\n.*@(?:unnum|sub)[^ ]* (.*)\n.*@translationof (.*)\n' "'\2': '\1',\n" \
        | grep -E 'Source|Learning|Glossary|Essay|Notation|Usage|Snippets|Web|Changes|Extending|Internals|Contributor'
    echo "},"
done
'''

translations = {
    'de': {
        'Source': 'Quellen',
        'Learning': 'Einführung',
        'Music glossary': 'Glossar',
        'Essay': 'Aufsatz',
        'Notation': 'Notation',
        'Usage': 'Benutzung',
        'Snippets': 'Schnipsel',
        'Web': 'Web',
        'Changes': 'Änderungen',

        'Extending': 'Erweitern',
        'Internals': 'Interna',
        'Contributor': 'Beitragen',

        ' (split HTML)': ' (geteiltes HTML)',
        ' (big HTML)': ' (großes HTML)',

        'Regression tests for ': 'Regressionstests für ',
        'PDF of regtests for ': 'PDF der Regressionstests für ',
        'MusicXML Regression tests for ': 'MusicXML Regressionstests für ',
        'PDF of MusicXML regtests for ': 'PDF der MusicXML Regressionstests für ',

        'Doc tarball for ': 'Dokumentation tar-gepackt für ',
        ' (did not exist in 2.12)': ' (nicht existent in 2.12)',
        },
    'es': {
        'Source': 'Código fuente',

        'Learning': 'Aprendizaje',
        'Music glossary': 'Glosario',
        'Essay': 'Ensayo',
        'Notation': 'Notación',
        'Usage': 'Utilización',
        'Snippets': 'Fragmentos',
        'Web': 'Web',
        'Changes': 'Cambios',
        'Extending': 'Extensión',
        'Internals': 'Funcionamiento interno',
        'Contributor': 'Guía del colaborador',

# keep the spaces!
        ' (split HTML)': ' (HTML seccionado)',
        ' (big HTML)': ' (HTML monolítico)',

        'Regression tests for ': 'Pruebas de regresión para ',
        'PDF of regtests for ': 'Pruebas en PDF para ',
        'MusicXML Regression tests for ': 'Pruebas de regresión de MusicXML para ',
        'PDF of MusicXML regtests for ': 'Pruebas de MusicXML en PDF para ',

        'Doc tarball for ': 'Tarball de la documentación para ',
        ' (did not exist in 2.12)': ' (no existía en la versión 2.12)',
     },
    'fr': {
        'Source': 'Sources',

        'Learning': 'Initiation',
        'Music glossary': 'Glossaire',
        'Essay': 'Essai',
        'Notation': 'Notation',
        'Usage': 'Utilisation',
        'Snippets': 'Morceaux choisis',
        'Web': 'Web',
        'Changes': 'Nouveautés',
        'Extending': 'Extension',
        'Internals': 'Propriétés internes',
        'Contributor': 'Guide du contributeur',

# keep the spaces!
        ' (split HTML)': ' (HTML multipages)',
        ' (big HTML)': ' (HTML en page unique)',

        'Regression tests for ': 'Tests de régression pour ',
        'PDF of regtests for ': 'PDF des tests de régression pour ',
        'MusicXML Regression tests for ': 'Tests de régression de MusicXML pour ',
        'PDF of MusicXML regtests for ': 'PDF des tests de régression de MusicXML pour ',

        'Doc tarball for ': 'Archive de la documentation pour ',
        ' (did not exist in 2.12)': ' (non disponible pour la version 2.12)',
        },
    'hu': {
        'Source': 'Forrás',
        'Learning': 'Tankönyv',
        'Music glossary': 'Fogalomtár',
        'Essay': 'Esszé',
        'Notation': 'Kottaírás',
        'Usage': 'Használat',
        'Snippets': 'Kódrészletek',
        'Web': 'Web',
        'Changes': 'Változások',

        #TODO
        'Extending': 0,
        'Internals': 'Belső működés',
        'Contributor': 0,

        ' (split HTML)': 0,
        ' (big HTML)': 0,

        'Regression tests for ': 0,
        'PDF of regtests for ': 0,
        'MusicXML Regression tests for ': 0,
        'PDF of MusicXML regtests for ': 0,

        'Doc tarball for ': 0,
        ' (did not exist in 2.12)': 0,
        },
    'ja': {
        'Source': 'ソース',
        'Learning': '学習',
        'Music glossary': '用語集',
        'Essay': 'エッセー',
        'Notation': '記譜法',
        'Usage': '使用方法',
        'Snippets': 'コード断片集',
        'Web': 'Web',
        'Changes': '変更点',

        #TODO
        'Extending': '拡張',
        'Internals': '内部リファレンス',
        'Contributor': '貢献者向けガイド',

# keep the spaces!
        ' (split HTML)': ' (ページ毎に分割された HTML)',
        ' (big HTML)': ' (1 つの大きな HTML)',

        'Regression tests for ': '回帰テスト バージョン ',
        'PDF of regtests for ': '回帰テスト (PDF 版) バージョン ',
        'MusicXML Regression tests for ': 'MusicXML 回帰テスト バージョン ',
        'PDF of MusicXML regtests for ': 'MusicXML 回帰テスト (PDF 版) バージョン ',

        'Doc tarball for ': 'ドキュメント アーカイブ バージョン ',
        ' (did not exist in 2.12)': ' (バージョン 2.12 には存在しません)',

        },
    'nl': {
        'Source': 'Broncode',

        'Learning': 'Beginnen',
        'Music glossary': 'Terminologie',
        'Essay': 'Essay',
        'Notation': 'Notatie',
        'Usage': 'Gebruik',
        'Snippets': 'Snippers',
        'Web': 'Web',
        'Changes': 'Veranderingen',
        'Extending': 'Uitbreidingen',
        'Internals': 'Internals',
        'Contributor': 'Contributor',

# keep the spaces!
        ' (split HTML)': ' (opgesplitste HTML)',
        ' (big HTML)': ' (grote pagina HTML)',

        'Regression tests for ': 'Regressietesten voor ',
        'PDF of regtests for ': 'PDF van regressietesten voor ',
        'MusicXML Regression tests for ': 'MusicXML regressietesten voor ',
        'PDF of MusicXML regtests for ': 'MusicXML regressietesten voor ',

        'Doc tarball for ': 'Tarball met documentation voor ',
        ' (did not exist in 2.12)': ' (bestond nog niet in 2.12)',
     },
    }




### actual program


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

def _ (string, lang):
    return translations.get (lang.split ('_')[0], {}).get (string, string)

getTrans = _
# let's not barf, but print a warning when something's missing
def getTrans(text, lang):
    trans = _ (text, lang)
    if not trans:
        trans = text
        sys.stderr.write ('create-weblinks-itexi: warning: [%(lang)s]: translation missing for: %(text)s\n' % locals ())
    return trans

def macroLang(name, lang):
    if (lang != ''):
        return name + '-' + lang
    return name

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

def make_download_source(name, vstring, version, lang):
    string = "@uref{http://download.linuxaudio.org/lilypond/sources/"
    string += vstring + "/"
    string += "lilypond-" + version + ".tar.gz"
    string += ", "
    string += getTrans("Source", lang)
    string += ": lilypond-" + version + ".tar.gz"
    string += "}"
    make_macro(macroLang(name,lang), string)

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

def addLang(url, lang):
    if lang:
        base, ext = os.path.splitext(url)
        return base + '.' + lang + ext
    else:
        return url

def make_manual_links(name, version, lang):
    """Here is where all the macros manualStableLearningSplit,
    manualStableLearningBig, manualStableLearningSplitNoName, etc. are
    created on the fly.  Hopefully this documentation string will help
    others a bit while grepping for those.
    """
    for m in manuals:
        manual = m
        # TODO: this is a stupid way of doing it
        if (m=='music-glossary'):
            mshort = 'Glossary'
        else:
            mshort = m.capitalize()
        if (manual=='music-glossary'):
            manual = 'Music glossary'
        url = translateNameToUrl(m, version)

        if (url == ''):
            # can't have a comma here due to texinfo
            make_ver_link(macroLang("manual"+name+mshort+'Pdf',lang),
                "http://lilypond.org",
                mshort+getTrans(" (did not exist in 2.12)",lang))
            make_ver_link(macroLang("manual"+name+mshort+'Split',lang),
                "http://lilypond.org",
                mshort+getTrans(" (did not exist in 2.12)",lang))
            make_ver_link(macroLang("manual"+name+mshort+'Big',lang),
                "http://lilypond.org",
                mshort+getTrans(" (did not exist in 2.12)",lang))
            make_ver_link(macroLang("manual"+name+mshort+'SplitNoName',lang),
                "http://lilypond.org",
                mshort+getTrans(" (did not exist in 2.12)",lang))
            continue
	# this is stupid and I shouldn't have bothered trying
	# to support the 2.12 docs and it will be deleted once
	# 2.14 is out and the website won't be visible to users
        # until 2.14 is out.  -gp
        if (url.endswith('.html')):
            make_ver_link(macroLang("manual"+name+mshort+'Pdf',lang),
                      addLang(url, lang),
                      getTrans(manual.capitalize(),lang) + '.pdf')
            make_ver_link(macroLang("manual"+name+mshort+'Split',lang),
                      addLang(url, lang),
                      getTrans(manual.capitalize(),lang) +
                      getTrans(' (split HTML)',lang))
            make_ver_link(macroLang("manual"+name+mshort+'Big',lang),
                      addLang(url, lang),
                      getTrans(manual.capitalize(),lang) +
                      getTrans(' (big HTML)',lang))
            newurl = url
        else:
            make_ver_link(macroLang("manual"+name+mshort+'Pdf',lang),
                      # TODO: this is an even stupider way of doing it
                      addLang(url+'.pdf', lang),
                      getTrans(manual.capitalize(),lang) + '.pdf')
            make_ver_link(macroLang("manual"+name+mshort+'Split',lang),
                      addLang(url + '/index.html', lang),
                      getTrans(manual.capitalize(),lang) +
                      getTrans(' (split HTML)',lang))
            make_ver_link(macroLang("manual"+name+mshort+'Big',lang),
                      addLang(url + '-big-page.html', lang),
                      getTrans(manual.capitalize(),lang) +
                      getTrans(' (big HTML)',lang))
            newurl = url + '/index.html'
        make_ver_link(macroLang("manual"+name+mshort+'SplitNoName',lang),
                  addLang(newurl, lang),
                  getTrans(manual.capitalize(),lang))

def make_regtest_links(name, version, lang):
    ver_split = version.split('.')
    ver_minor = ver_split[0] + '.' + ver_split[1]
    url = depth + "doc/v" + ver_minor + "/input/regression/"

    make_ver_link(macroLang("regtest"+name, lang),
        url+"collated-files.html",
        getTrans("Regression tests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"Pdf", lang),
        url+"collated-files.pdf",
        getTrans("PDF of regtests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"Xml", lang),
        url+"musicxml/collated-files.html",
        getTrans("MusicXML Regression tests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"Abc", lang),
        url+"abc2ly/collated-files.html",
        getTrans("abc2ly Regression tests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"LilypondBook", lang),
        url+"lilypond-book/collated-files.html",
        getTrans("lilypond-book Regression tests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"XmlPdf", lang),
         url+"musicxml/collated-files.pdf",
        getTrans("PDF of MusicXML regtests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"AbcPdf", lang),
         url+"abc2ly/collated-files.pdf",
        getTrans("PDF of abc2ly regtests for ", lang)+version)
    make_ver_link(macroLang("regtest"+name+"LilypondBookPdf", lang),
         url+"lilypond-book/collated-files.pdf",
        getTrans("PDF of lilypond-book regtests for ", lang)+version)

def make_doctarball_links(name, version, lang):
    url = depth + "download/binaries/documentation/lilypond-"
    # ugly FIXME, but proper build number support isn't Critical.
    url += version + "-1"
    url += ".documentation.tar.bz2"
    make_ver_link(macroLang("doctarball"+name, lang),
        url, getTrans("Doc tarball for ", lang)+version)

print "@c ************************ Download binaries ************"
make_all_downloads("Stable", VERSION_STABLE)
make_all_downloads("Devel", VERSION_DEVEL)

print "@c ************************ Download source ************"
# FIXME: icky hard-coding!  -gp
for lang in langs:
    print "@c *********", lang, "***"
    make_download_source("downloadStableSource","v2.12",VERSION_STABLE,lang)
    make_download_source("downloadDevelSource","v2.13",VERSION_DEVEL,lang)

print "@c ************************ Manual links ************"
for lang in langs:
    print "@c *********", lang, "***"
    make_manual_links("Stable", VERSION_STABLE,lang)
    make_manual_links("Devel", VERSION_DEVEL,lang)

    make_doctarball_links("Stable", VERSION_STABLE,lang)
    make_doctarball_links("Devel", VERSION_DEVEL,lang)

print "@c ************************ Regtest links ************"
for lang in langs:
    print "@c *********", lang, "***"
    make_regtest_links("Stable", VERSION_STABLE,lang)
    make_regtest_links("Devel", VERSION_DEVEL,lang)


