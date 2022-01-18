#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2008--2022 John Mandereau <john.mandereau@gmail.com>
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


# Temporary script that helps translated docs sources conversion
# for texi2html processing

# USAGE:  tely-gettext.py LANG FILES


import langdefs
import os
import re
import sys
print("tely-gettext.py")


lang = sys.argv[1]
files = sys.argv[2:]

_doc = langdefs.translation[lang]

include_re = re.compile(r'@include (.*?)$', re.M)
whitespaces = re.compile(r'\s+')
ref_re = re.compile(
    r'(?ms)@((?:ressay|rgloss|rinternals|rlearning|rslr|rprogram|ruser|ref)|named)\{(.*?)\}')
node_section_re = re.compile(
    r'@node (.*?)\n@((?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading) (.*?)\n')
section_only_re = re.compile(
    r'@((?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading) (.*?)\n')
menu_entry_re = re.compile(r'\* (.*?)::')
untranslated_node_re = re.compile(
    r'(@node\s+.*\n@.*\n.*)(\s+@untranslated(.|\n)+?)(?=@node|@subheading|@menu|$)')


def ref_gettext(m):
    r = whitespaces.sub(' ', m.group(2))
    return '@' + m.group(1) + '{' + _doc(r) + '}'


def node_gettext(m):
    return '@node ' + _doc(m.group(1)) + '\n@' + \
        m.group(2) + ' ' + _doc(m.group(3)) + \
        '\n@translationof ' + m.group(1) + '\n'


def section_gettext(m):
    return '@' + m.group(1) + ' ' + _doc(m.group(2)) + '\n'


def menu_entry_gettext(m):
    return '* ' + _doc(m.group(1)) + '::'


def process_file(filename, master_file_dir='.', included=False):
    print("Processing %s" % filename)
    f = open(filename, 'r', encoding='utf-8')
    page = f.read()
    f.close()
    page = ref_re.sub(ref_gettext, page)
    if not '\\n@translationof' in page:
        page = node_section_re.sub(node_gettext, page)
    page = section_only_re.sub(section_gettext, page)
    page = menu_entry_re.sub(menu_entry_gettext, page)
    page = page.replace("""@c -- SKELETON FILE --
""", '')
    page = page.replace('UNTRANSLATED NODE: IGNORE ME', '@untranslated')
    page = untranslated_node_re.sub('\\1 @c external\\2', page)
    includes = include_re.findall(page)
    f = open(filename, 'w', encoding='utf-8')
    f.write(page)
    f.close()
    dir = os.path.dirname(filename)
    if not included:
        master_file_dir = dir
    for file in includes:
        p = os.path.join(dir, file)
        if os.path.exists(p):
            process_file(p, master_file_dir, included=True)
        else:
            p = os.path.join(master_file_dir, file)
            if os.path.exists(p):
                process_file(p, master_file_dir, included=True)


for filename in files:
    process_file(filename)
