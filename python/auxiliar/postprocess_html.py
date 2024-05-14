#!@PYTHON@
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2023 John Mandereau <john.mandereau@gmail.com>
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
add footer, tweak links, add language selection menu and syntax highlighting toggle.
"""
import re
import os
import sys
import time

import langdefs
from typing import Dict, List, Tuple, Optional, Any


def _doc(s: str) -> str:
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

lang_available = _doc("Other languages: %s.")
browser_lang = _doc('About <a href="%s">automatic language selection</a>.')
browser_language_url = "http://www.lilypond.org/misc/browser-language"
syntax_highlighting_disable = _doc("Disable syntax highlighting")
syntax_highlighting_enable = _doc("Enable syntax highlighting")
syntax_highlighting_save = _doc("Save syntax highlighting preference")
syntax_highlighting_warn_local_storage = _doc("""Remembering your preference regarding \
syntax highlighting for later visits on this documentation necessitates storing \
the preference in cookie-like browser storage.  This information remains local \
to your device and is never accessed by lilypond.org.""")


LANGUAGES_TEMPLATE = '''
<p id="languages">
%(language_available)s
<br>
%(browser_language)s
</p>
'''

html_re = re.compile('(.*?)(?:[.]([^/.]*))?[.]html$')


class Processor:
    """Encapsulates the list of translations, grouped by page."""

    def __init__(self, filelist: List[str]):
        # basename => list of languages dict
        self._pages_dict: Dict[str, List[str]] = {}

        language_codes = set([l.webext for l in langdefs.LANGUAGES])
        for f in filelist:
            m = html_re.match(f)
            if not m:
                continue

            g = m.groups()
            if len(g) <= 1 or g[1] is None:
                e = ''
            else:
                e = g[1]

            if e not in language_codes:
                continue

            if not g[0] in self._pages_dict:
                self._pages_dict[g[0]] = [e]
            else:
                self._pages_dict[g[0]].append(e)

    def todo_items(self) -> List[Tuple[str, List[str]]]:
        return list(self._pages_dict.items())

    def find_translations(self, prefix: str, lang_ext: str) -> List[langdefs.LanguageDef]:
        """find available translations of a page"""
        available = []
        for l in langdefs.LANGUAGES:
            e = l.webext
            if lang_ext != e:
                if e in self._pages_dict[prefix]:
                    available.append(l)
        return available

    def process_links(self, content: str, prefix: str, lang_ext: str, is_online: bool) -> str:
        if is_online:
            # Strip .html, suffix for auto language selection (content
            # negotiation).  The menu must keep the full extension, so do
            # this before adding the menu.
            return links_re.sub(process_online_link, content)
        else:
            def repl(m: re.Match):
                return self._process_offline_link(m, prefix, lang_ext)
            return links_re.sub(repl, content)

    def _process_offline_link(self, m: re.Match, prefix: str, lang_ext: str) -> str:
        destination_path = os.path.normpath(os.path.join(
            os.path.dirname(prefix),
            m.group(1)))
        if not (destination_path in self._pages_dict and
                lang_ext in self._pages_dict[destination_path]):
            lang_ext = ''

        anchor = remove_unneeded_anchor(m)
        if lang_ext != '':
            lang_ext = '.' + lang_ext
        return ('href="' + m.group(1) + lang_ext
                + '.html' + anchor + '"')


footer_insert_re = re.compile(r'<!--\s*FOOTER\s*-->')
end_body_re = re.compile(r'(?i)</body>')


def add_footer(s: str, footer_text: str) -> str:
    """add footer"""

    # prefer inserting at FOOTER; this is necessary for the footer to
    # be positioned correctly relative to the nav frame in the manuals.
    (s, n) = footer_insert_re.subn(footer_text + '\n' + '<!-- FOOTER -->',
                                   s, 1)
    if not n:
        (s, n) = end_body_re.subn(footer_text + '\n' + '</body>', s, 1)
    if not n:
        # this happens with the HTML files under Documentation/misc
        s += footer_text
    return s


links_re = re.compile(r'href="([^/]\.*[^".]*)\.html(#[^"]*|)"')


def remove_unneeded_anchor(m: re.Match) -> str:
    file_no_ext = m.group(1)
    anchor = m.group(2)
    if anchor != '' and file_no_ext.endswith(anchor[1:]):
        return ''
    return anchor


def process_online_link(m: re.Match) -> str:
    anchor = remove_unneeded_anchor(m)
    return ('href="' + m.group(1) + anchor + '"')


# About the @license comments, see
# https://www.gnu.org/software/librejs/free-your-javascript.html
syntax_highlighting_code = '''
<script type='text/javascript'>
    // @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&dn=gpl-3.0.txt GPL-v3-or-Later
    (function () {
        let handled = false;
        function restoreHighlighting () {
            if (handled) return;
            if (localStorage.getItem('syntax-highlighting') !== 'no') {
            enable_highlighting();
            }
            else {
              disable_highlighting();
            }
            handled = true;
        }

        // Attempt early handling but fall back to onload just in case.
        document.addEventListener('DOMContentLoaded', restoreHighlighting);
        document.addEventListener('load', restoreHighlighting);
    })();

    let enabled = true;
    function enable_highlighting() {
        document.body.classList.add('highlight');
        enabled = true;
        document.getElementById('highlighting-active-link').innerHTML =
            "<a href='javascript:disable_highlighting()'>%(disable_text)s<\\/a>";
    }
    function disable_highlighting() {
        document.body.classList.remove('highlight');
        enabled = false;
        document.getElementById('highlighting-active-link').innerHTML =
            "<a href='javascript:enable_highlighting()'>%(enable_text)s<\\/a>";
    }

    function save_preference() {
        if (confirm("%(warn_local_storage_text)s")) // EU law compliance
            localStorage.setItem('syntax-highlighting', enabled ? 'yes' : 'no');
    }
    // @license-end
</script>

<div id="highlighting-settings">
  <a id="highlighting-active-link"></a>
  &ndash;
  <a href='javascript:save_preference()'>%(save_text)s</a>
</div>
'''


def add_menu(page_content: str, prefix: str, lang_ext: str, available: List[langdefs.LanguageDef], translation) -> str:
    language_menu = ''
    if lang_ext != '':
        t = translation[lang_ext]
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

    if 'web' not in prefix:
        disable = t(syntax_highlighting_disable)
        enable = t(syntax_highlighting_enable)
        save = t(syntax_highlighting_save)
        warn = t(syntax_highlighting_warn_local_storage)
        code = syntax_highlighting_code % {"disable_text": disable,
                                           "enable_text": enable,
                                           "save_text": save,
                                           "warn_local_storage_text": warn}
        full_footer += code

    full_footer = '''<div id="footer">%s</div>''' % full_footer

    return add_footer(page_content, full_footer)


def process_html_files(processor: Processor,
                       todo_items: List[Tuple[str, List[str]]],
                       package_name='',
                       package_version='',
                       dest_dir='',
                       is_online=False):
    """Add header, footer, syntax highlighting toggle, and tweak links
    to a number of HTML files.

    Arguments:
     processor :               Processor instance
     package_name=NAME         set package_name to NAME
     package_version=VERSION   set package version to VERSION
     is_online                 set page processing for HTTP webserving with content
                                 negotiation rather filesystem browsing
     dest_dir:                 where to write output; if unset, in-place editing..
    """
    versiontup = package_version.split('.')
    branch_str = _doc('stable-branch')
    if int(versiontup[1]) % 2:
        branch_str = _doc('development-branch')

    en_dict = {
        'package_name': package_name,
        'package_version': package_version,
        'branch_str': branch_str,
        'help_us_url': 'https://lilypond.org/help-us.html',
        'bug_lilypond_url': 'https://lists.gnu.org/mailman/listinfo/bug-lilypond',
        'footer_name_version':  _doc('This page is for %(package_name)s-'
                                     '%(package_version)s (%(branch_str)s).'),
        'footer_report_links': _doc('We welcome your aid; please '
                                    '<a href="%(help_us_url)s">help us</a> by '
                                    'reporting errors to our '
                                    '<a href="%(bug_lilypond_url)s">bug list</a>.'),
    }

    # language => (dict of str => str)
    subst = {
        '': en_dict,
    }
    for l in langdefs.translation:
        e = langdefs.LANGDICT[l].webext
        if e:
            subst[e] = {
                name: langdefs.translation[l](en_dict[name])
                for name in en_dict}

    # Do deeper string formatting as early as possible,
    # so only one '%' formatting pass is needed later
    for e in subst:
        for k in ['footer_name_version', 'footer_report_links']:
            subst[e][k] = subst[e][k] % subst[e]

    for prefix, ext_list in todo_items:
        for lang_ext in ext_list:
            file_name = langdefs.lang_file_name(prefix, lang_ext, '.html')
            dest_time = 0

            content = open(file_name, 'r', encoding='utf-8').read()
            content = content.replace('%', '%%')

            # add sidebar information
            content = content.replace(
                '<!-- Sidebar Version Tag -->', sidebar_version)

            available = processor.find_translations(prefix, lang_ext)
            page_content = processor.process_links(
                content, prefix, lang_ext, is_online)
            # Add menu after stripping: must not have autoselection for language menu.
            page_content = add_menu(
                page_content, prefix, lang_ext, available, langdefs.translation)

            page_content = page_content % subst[lang_ext]

            # Must write to tmp file to avoid touching hardlinked files.
            dest = os.path.join(dest_dir, file_name)
            if dest_dir:
                os.makedirs(os.path.dirname(dest), exist_ok=True)
            out_f = open(dest + ".tmp", 'w', encoding='utf-8')
            out_f.write(page_content)
            out_f.close()
            os.rename(dest + ".tmp", dest)

        # if the page is translated, a .en.html symlink is necessary for content negotiation
        if is_online and ext_list != [''] and not os.path.lexists(prefix + '.en.html'):
            os.symlink(os.path.basename(prefix) + '.html', prefix + '.en.html')
