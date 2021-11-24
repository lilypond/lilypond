#!@PYTHON@
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2022 John Mandereau <john.mandereau@gmail.com>
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

lang_available = _doc("Other languages: %s.")
browser_lang = _doc('About <a href="%s">automatic language selection</a>.')
browser_language_url = "http://www.lilypond.org/website/misc/browser-language"
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


def build_pages_dict(filelist):
    """Build dictionary of available translations of each page.

    Returns: basename => list of languages dict"""
    pages_dict = {}

    language_codes = set([l.webext for l in langdefs.LANGUAGES])
    for f in filelist:
        m = html_re.match(f)
        if m:
            g = m.groups()
            if len(g) <= 1 or g[1] is None:
                e = ''
            else:
                e = g[1]

            if e not in language_codes:
                continue

            if not g[0] in pages_dict:
                pages_dict[g[0]] = [e]
            else:
                pages_dict[g[0]].append(e)
    return pages_dict


footer_insert_re = re.compile(r'<!--\s*FOOTER\s*-->')
end_body_re = re.compile(r'(?i)</body>')
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


def find_translations(pages_dict, prefix, lang_ext):
    """find available translations of a page"""
    available = []
    for l in langdefs.LANGUAGES:
        e = l.webext
        if lang_ext != e:
            if e in pages_dict[prefix]:
                available.append(l)
    return available


online_links_re = re.compile('''(href|src)=['"]\
((?!Compiling-from-source.html")[^/][.]*[^.:'"]*)\
([.]html)(#[^"']*|)['"]''')
offline_links_re = re.compile('''href=['"]\
((?!Compiling-from-source.html")(?![.]{2}/contributor)[^/][.]*[^.:'"]*)\
([.]html)(#[^"\']*|)[\'"]''')
big_page_name_re = re.compile('''(.+?)-big-page''')

def process_i18n_links(pages_dict, match, prefix, lang_ext):
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


def process_links(pages_dict, content, prefix, lang_ext, file_name, target):
    page_flavors = {}
    if target == 'online':
        # Strip .html, suffix for auto language selection (content
        # negotiation).  The menu must keep the full extension, so do
        # this before adding the menu.
        page_flavors[file_name] = [lang_ext,
                                   online_links_re.sub('\\1="\\2\\4"', content)]
    elif target == 'offline':
        if lang_ext == '':
            page_flavors[file_name] = [lang_ext, content]
        else:
            page_flavors[file_name] = [lang_ext,
                                       offline_links_re.sub(
                                           lambda match:
                                           process_i18n_links(
                                               pages_dict,
                                               match, prefix, lang_ext),
                                           content)]
    return page_flavors

# About the @license comments, see
# https://www.gnu.org/software/librejs/free-your-javascript.html
syntax_highlighting_code = '''
<script>
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
            "<a href='javascript:disable_highlighting()'>%(disable_text)s</a>";
    }
    function disable_highlighting() {
        document.body.classList.remove('highlight');
        enabled = false;
        document.getElementById('highlighting-active-link').innerHTML =
            "<a href='javascript:enable_highlighting()'>%(enable_text)s</a>";
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

        page_flavors[k][1] = add_footer(page_flavors[k][1], full_footer)
    return page_flavors


def process_html_files(pages_dict,
                       package_name='',
                       package_version='',
                       target='offline'):
    """Add header, footer, syntax highlighting toggle, and tweak links
    to a number of HTML files.

    Arguments:
     pages_dict:               dict of filename => translations
     package_name=NAME         set package_name to NAME
     package_version=VERSION   set package version to VERSION
     targets=offline|online    set page processing depending on the target
          offline is for reading HTML pages locally
          online is for hosting the HTML pages on a website with content
            negotiation
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
        'footer_name_version':  _doc ('This page is for %(package_name)s-'
                                      '%(package_version)s (%(branch_str)s).'),
        'footer_report_links': _doc ('We welcome your aid; please '
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

    for prefix, ext_list in list(pages_dict.items()):
        for lang_ext in ext_list:
            file_name = langdefs.lang_file_name(prefix, lang_ext, '.html')
            dest_time = 0

            content = open(file_name, 'r', encoding='utf-8').read()
            content = content.replace('%', '%%')

            # add sidebar information
            content = content.replace('<!-- Sidebar Version Tag  -->', sidebar_version)

            available = find_translations(pages_dict, prefix, lang_ext)
            page_flavors = process_links(pages_dict,
                                         content, prefix, lang_ext, file_name, target)
            # Add menu after stripping: must not have autoselection for language menu.
            page_flavors = add_menu(
                page_flavors, prefix, available, target, langdefs.translation)

            for k in page_flavors:
                page_flavors[k][1] = page_flavors[k][1] % subst[page_flavors[k][0]]

                # Must write to tmp file to avoid touching hardlinked files.
                out_f = open(k + ".tmp", 'w', encoding='utf-8')
                out_f.write(page_flavors[k][1])
                out_f.close()
                os.rename(k + ".tmp", k)

        # if the page is translated, a .en.html symlink is necessary for content negotiation
        if target == 'online' and ext_list != [''] and not os.path.lexists(prefix + '.en.html'):
            os.symlink(os.path.basename(prefix) + '.html', prefix + '.en.html')
