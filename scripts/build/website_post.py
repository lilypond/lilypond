#!@PYTHON@
#-*- coding: utf-8 -*-

##### This is web_post.py. This script deals with translations
##### in the "make website" target.

import sys
import os
import glob
import re

###### Translation data
lang_lookup = {
    'fr': 'français',
    'es': 'español',
    '': 'english'
}

lang_other_langs = {
    'es': 'Otros idiomas: ',
    'fr': 'Autres langues : ',
    '': 'Other languages: '
}

exclude_pages = [
    'music-glossary',
    'snippets',
    'internals',
    'contributor'
]

###### Actual program

dir = sys.argv[1]

os.chdir(dir)
html_files = glob.glob( '*.html' )


### messy way to get all languages
langs_set = set()
for file in html_files:
    file_split = file.split('.')
    if (len(file_split) == 2):
        # it's English
        lang = ''
    elif (len(file_split) == 3):
        # it's a translation
        lang = file_split[1]
        # make sure it's a translated language
        if (not (lang == "en")):
            langs_set.add(lang)
langs = list(langs_set)
langs.sort()


### helper functions
def addLangExt(filename, lang, ext):
    text = filename
    if (not (lang=="")):
        text += "." + lang
    text += "." + ext
    return text

def makeFooter(filename, currentLang):
    text = "<p id=\"languages\">\n"
    text += lang_other_langs[currentLang]
    for i in range(len(langs)):
        lang = langs[i]
        if (lang == currentLang):
            continue
        text += "<a href=\""
	text += addLangExt(filename, lang, "html")
        text += "\">"
        text += lang_lookup[lang]
        text += "</a>"
        if (i < len(langs)-2):
            text += ", "
        else:
            text += ".\n"
    # TODO: add link to automatic language selection?
    # still need to include this page in the new webpages somewhere
    text += "</p>\n"
    return text

def getLocalHref(line):
    match = re.search(r'href=[\'"]?([^\'" >]+)', line)
    if match:
        url = match.group(0)[6:]
        if (url[0:7] == "http://"):
            url = ''
        # strip any '#'
        omit = url.find('#')
        if (omit >= 0):
            url = url[0:omit]
    else:
        url = ''
    return url




### main loop
for file in html_files:
    ### we want to strip the .html and get the lang
    file_split = file.split('.')
    file_base = os.path.basename( file_split[0] )
    if (len(file_split) == 2):
        # it's English
        lang = ''
        # possibly necessary for automatic language selection
        file_symlink = file.replace(".html", ".en.html")
        if (not (os.path.exists(file_symlink))):
            os.symlink (file, file_symlink)
    elif (len(file_split) == 3):
        # it's a translation
        lang = file_split[1]
        if (lang == "en"):
            # it's a symlink
            continue
    else:
        # it's a mess
        print "is a mess"
        continue

    ### we need to replace parts of the file
    lines = open(file).readlines()
    os.remove(file)
    outfile = open(file, 'w')

    lang_footer = makeFooter(file_base, lang)


    ### replace links as appropraite
    for line in lines:
        link = getLocalHref(line)
        if (link != ""):
            link_base = link.split('.')[0]
            if (line.endswith(".html")):
	        langlink = addLangExt(link_base, lang, "html")
                line.replace(link, langlink)
            if (line.endswith(".pdf")):
	        langlink = addLangExt(link_base, lang, "pdf")
        if (line.find("<!-- FOOTER -->") >= 0):
            outfile.write( lang_footer )
        outfile.write(line)
    outfile.close()

