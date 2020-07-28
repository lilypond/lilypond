#!@PYTHON@
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2020 John Mandereau <john.mandereau@gmail.com>
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


"""
Postprocess HTML files:
add footer, tweak links, add language selection menu.
"""
import codecs
import re
import os
import sys
import time
import operator

import langdefs
from functools import reduce

# This is to try to make the docball not too big with almost duplicate files
# see process_links()
non_copied_pages = ['Documentation/out-www/notation-big-page',
                    'Documentation/out-www/internals-big-page',
                    'Documentation/out-www/learning-big-page',
                    'Documentation/out-www/usage-big-page',
                    'Documentation/out-www/music-glossary-big-page',
                    'Documentation/out-www/contributor',
                    'Documentation/out-www/changes-big-page',
                    'Documentation/out-www/essay-big-page',
                    'Documentation/out-www/extending-big-page',
                    'Documentation/out-www/snippets',
                    'out-www/examples',
                    'Documentation/topdocs',
                    'Documentation/bibliography',
                    'Documentation/out-www/DEDICATION',
                    'input/']


def _doc(s):
    return s


program_name = os.path.basename(sys.argv[0])

footer = '''
<p class="footer_version">
%(footer_name_version)s
</p>
<p class="footer_report">
%(footer_report_links)s
</p>
'''


footer_name_version = _doc('This page is for %(package_name)s-'
                           '%(package_version)s (%(branch_str)s).')
# ugh, must not have "_doc" in strings because it is naively replaced with "_"
# in hacked gettext process
footer_report_links = _doc('We welcome your aid; please '
                           '<a href="%(help_us_url)s">help us</a> by '
                           'reporting errors to our '
                           '<a href="%(bug_lilypond_url)s">bug list</a>.')
sidebar_version = _doc(' v%(package_version)s (%(branch_str)s).')

bug_lilypond_url = 'https://lists.gnu.org/mailman/listinfo/bug-lilypond'
help_us_url = 'https://lilypond.org/help-us.html'

header_tag = '<!-- header_tag -->'
header_tag_re = re.compile(header_tag)

lang_available = _doc("Other languages: %s.")
browser_lang = _doc('About <a href="%s">automatic language selection</a>.')
browser_language_url = "http://www.lilypond.org/website/misc/browser-language"

LANGUAGES_TEMPLATE = '''
<p id="languages">
%(language_available)s
<br>
%(browser_language)s
</p>
'''


html_re = re.compile('(.*?)(?:[.]([^/.]*))?[.]html$')
pages_dict = {}


def build_pages_dict(filelist):
    """Build dictionary of available translations of each page"""
    global pages_dict
    for f in filelist:
        m = html_re.match(f)
        if m:
            g = m.groups()
            if len(g) <= 1 or g[1] is None:
                e = ''
            else:
                e = g[1]
            if not g[0] in pages_dict:
                pages_dict[g[0]] = [e]
            else:
                pages_dict[g[0]].append(e)


body_tag_re = re.compile('(?i)<body([^>]*)>')
html_tag_re = re.compile('(?i)<html>')
doctype_re = re.compile('(?i)<!DOCTYPE')
doctype = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n'
css_re = re.compile('(?i)<link ([^>]*)href="[^">]*?'
                    '(lilypond.*\.css)"([^>]*)>')
end_head_tag_re = re.compile('(?i)</head>')
css_link = ('    <link rel="stylesheet" type="text/css" title="Default design"'
            ' href="%(rel)sDocumentation/css/lilypond-manuals.css">\n'
            '    <!--[if lte IE 7]>\n'
            '    <link href="%(rel)sDocumentation/css/lilypond-ie-fixes.css"'
            ' rel="stylesheet" type="text/css">\n'
            '    <![endif]-->\n')


def add_header(s, prefix):
    """Add header (<body>, doctype and CSS)"""
    header = ""
    if header_tag_re.search(s) == None:
        body = '<body\\1>'
        (s, n) = body_tag_re.subn(body + header, s, 1)
        if not n:
            (s, n) = html_tag_re.subn('<html>' + header, s, 1)
            if not n:
                s = header + s

        if doctype_re.search(s) is None:
            s = doctype + header_tag + '\n' + s

        if css_re.search(s) is None:
            depth = (prefix.count('/') - 1) * '../'
            s = end_head_tag_re.sub((css_link % {'rel': depth})
                                    + '</head>', s)
    return s


footer_insert_re = re.compile('<!--\s*FOOTER\s*-->')
end_body_re = re.compile('(?i)</body>')
verifier_div = '<div id="verifier_texinfo">'
verifier_re = re.compile(verifier_div)


def add_footer(s, footer_text):
    """add footer"""

    # prefer inserting at FOOTER; this is necessary for the footer to
    # be positioned correctly relative to the nav frame in the manuals.
    (s, n) = footer_insert_re.subn(footer_text + '\n' + '<!-- FOOTER -->',
                                   s, 1)
    if not n:
        (s, n) = verifier_re.subn(footer_text + '\n' + verifier_div, s, 1)
    if not n:
        (s, n) = end_body_re.subn(footer_text + '\n' + '</body>', s, 1)
    if not n:
        # this happens with the HTML files under Documentation/misc
        s += footer_text
    return s


def find_translations(prefix, lang_ext):
    """find available translations of a page"""
    available = []
    missing = []
    for l in langdefs.LANGUAGES:
        e = l.webext
        if lang_ext != e:
            if e in pages_dict[prefix]:
                available.append(l)
            elif (lang_ext == '' and l.enabled and
                  reduce(operator.and_, [not prefix.startswith(s)
                                         for s in non_copied_pages])):
                # English version of missing translated pages will be written
                missing.append(e)
    return available, missing


online_links_re = re.compile('''(href|src)=['"]\
((?!Compiling-from-source.html")[^/][.]*[^.:'"]*)\
([.]html)(#[^"']*|)['"]''')
offline_links_re = re.compile('''href=['"]\
((?!Compiling-from-source.html")(?![.]{2}/contributor)[^/][.]*[^.:'"]*)\
([.]html)(#[^"\']*|)[\'"]''')
big_page_name_re = re.compile('''(.+?)-big-page''')


def process_i18n_big_page_links(match, prefix, lang_ext):
    big_page_name = big_page_name_re.match(match.group(1))
    if big_page_name:
        destination_path = os.path.normpath(os.path.join(
            os.path.dirname(prefix),
            big_page_name.group(0)))
        if not (destination_path in pages_dict and
                lang_ext in pages_dict[destination_path]):
            return match.group(0)
    return ('href="' + match.group(1) + '.' + lang_ext
            + match.group(2) + match.group(3) + '"')


def process_links(s, prefix, lang_ext, file_name, missing, target):
    page_flavors = {}
    if target == 'online':
        # Strip .html, suffix for auto language selection (content
        # negotiation).  The menu must keep the full extension, so do
        # this before adding the menu.
        page_flavors[file_name] = [lang_ext,
                                   online_links_re.sub('\\1="\\2\\4"', s)]
    elif target == 'offline':
        # in LANG doc index: don't rewrite .html suffixes
        # as not all .LANG.html pages exist;
        # the doc index should be translated and contain links
        # with the right suffixes
        # idem for NEWS
        if lang_ext == '':
            page_flavors[file_name] = [lang_ext, s]
        else:
            # For saving bandwidth and disk space, we don't duplicate big pages
            # in English, so we must process translated
            # big pages links differently.
            if 'big-page' in prefix:
                page_flavors[file_name] = [lang_ext,
                                           offline_links_re.sub(
                                               lambda match:
                                               process_i18n_big_page_links(
                                                   match, prefix, lang_ext),
                                               s)]
            else:
                page_flavors[file_name] = [lang_ext,
                                           offline_links_re.sub(
                                               'href="\\1.' + lang_ext
                                               + '\\2\\3"', s)]
    return page_flavors


def add_menu(page_flavors, prefix, available, target, translation):
    for k in page_flavors:
        language_menu = ''
        if page_flavors[k][0] != '':
            t = translation[page_flavors[k][0]]
        else:
            t = _doc
        for lang in available:
            lang_file = lang.file_name(os.path.basename(prefix), '.html')
            if language_menu != '':
                language_menu += ', '
            language_menu += '<a href="%s">%s</a>' % (lang_file, t(lang.name))

        languages = ''
        if language_menu:
            browser_language = t(browser_lang) % browser_language_url
            language_available = t(lang_available) % language_menu
            languages = LANGUAGES_TEMPLATE % vars()

        full_footer = ''
        if 'web' not in prefix:
            full_footer += footer

        full_footer += languages

        full_footer = '''<div id="footer">%s</div>''' % full_footer

        page_flavors[k][1] = add_footer(page_flavors[k][1], full_footer)
    return page_flavors


def process_html_files(package_name='',
                       package_version='',
                       target='offline'):
    """Add header, footer and tweak links to a number of HTML files

    Arguments:
     package_name=NAME         set package_name to NAME
     package_version=VERSION   set package version to VERSION
     targets=offline|online    set page processing depending on the target
          offline is for reading HTML pages locally
          online is for hosting the HTML pages on a website with content
            negotiation
    """
    translation = langdefs.translation
    localtime = time.strftime('%c %Z', time.localtime(time.time()))

    versiontup = package_version.split('.')
    branch_str = _doc('stable-branch')
    if int(versiontup[1]) % 2:
        branch_str = _doc('development-branch')

    # Initialize dictionaries for string formatting
    subst = {}
    subst[''] = dict([i for i in list(globals().items())
                      if isinstance(i[1], str)])
    subst[''].update(dict([i for i in list(locals().items())
                           if isinstance(i[1], str)]))
    for l in translation:
        e = langdefs.LANGDICT[l].webext
        if e:
            subst[e] = {}
            for name in subst['']:
                subst[e][name] = translation[l](subst[''][name])

    # Do deeper string formatting as early as possible,
    # so only one '%' formatting pass is needed later
    for e in subst:
        subst[e]['footer_name_version'] = (subst[e]['footer_name_version']
                                           % subst[e])
        subst[e]['footer_report_links'] = (subst[e]['footer_report_links']
                                           % subst[e])

    for prefix, ext_list in list(pages_dict.items()):
        for lang_ext in ext_list:
            file_name = langdefs.lang_file_name(prefix, lang_ext, '.html')
            dest_time = 0

            s = codecs.open(file_name, 'r', 'utf-8').read()
            s = s.replace('%', '%%')
            s = add_header(s, prefix)

            # add sidebar information
            s = s.replace('<!-- Sidebar Version Tag  -->', sidebar_version)

            available, missing = find_translations(prefix, lang_ext)
            page_flavors = process_links(
                s, prefix, lang_ext, file_name, missing, target)
            # Add menu after stripping: must not have autoselection for language menu.
            page_flavors = add_menu(
                page_flavors, prefix, available, target, translation)

            for k in page_flavors:
                page_flavors[k][1] = page_flavors[k][1] % subst[page_flavors[k][0]]

                # Must write to tmp file to avoid touching hardlinked files.
                out_f = codecs.open(k + ".tmp", 'w', 'utf-8')
                out_f.write(page_flavors[k][1])
                out_f.close()
                os.rename(k + ".tmp", k)

        # if the page is translated, a .en.html symlink is necessary for content negotiation
        if target == 'online' and ext_list != [''] and not os.path.lexists(prefix + '.en.html'):
            os.symlink(os.path.basename(prefix) + '.html', prefix + '.en.html')
