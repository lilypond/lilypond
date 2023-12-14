# langdefs.py
# -*- coding: utf-8 -*-
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
Documentation i18n module
"""

import gettext
import os
import re
import sys


def lang_file_name(p, langext, ext):
    if langext != '':
        return p + '.' + langext + ext
    return p + ext


class LanguageDef:
    def __init__(self, code, name, webext=None, html_filter=lambda s: s, enable_ly_identifier_l10n=True):
        self.code = code
        self.name = name
        self.enabled = True
        if webext is None:
            self.webext = self.code
        else:
            self.webext = webext
        self.html_filter = html_filter
        self.enable_ly_identifier_l10n = enable_ly_identifier_l10n

    def file_name(self, prefix, ext):
        return lang_file_name(prefix, self.webext, ext)


# All language information needed for documentation i18n is defined
# here. For each 'Documentation/ab' directory containing docs
# translated in 'ab', there should be one entry in LANGUAGES.

site = LanguageDef('en', 'English', webext='')

html_body_re = re.compile('<body.*?>', re.I)
html_end_body_re = re.compile('</body>', re.I)
french_html_typo_rules = ((' :', '&nbsp;:'),
                          (' ;', '&nbsp;;'),
                          (' ?', '<font size="-4">&nbsp;</font>?'),
                          (' !', '<font size="-4">&nbsp;</font>!'))


def french_html_filter(page):
    m = html_body_re.search(page)
    if m:
        body_begin = m.end()
    else:
        body_begin = 0
    m = html_end_body_re.search(page)
    if m:
        body_end = m.start()
    else:
        body_end = len(page)
    body = page[body_begin:body_end]
    for r in french_html_typo_rules:
        body = body.replace(r[0], r[1])
    return page[:body_begin] + body + page[body_end:]

ca = LanguageDef('ca', 'Català')
de = LanguageDef('de', 'Deutsch')
es = LanguageDef('es', 'Español')
fr = LanguageDef('fr', 'Français', html_filter = french_html_filter)
it = LanguageDef('it', 'Italiano')
ja = LanguageDef('ja', '日本語', enable_ly_identifier_l10n=False)
tr = LanguageDef('tr', 'Türkçe')
zh = LanguageDef('zh', '中文', enable_ly_identifier_l10n=False)

# Outdated or broken translations may be disabled
# (please run 'make doc-clean' before doing that):
#fr.enabled = False

LANGUAGES = (site, ca, de, es, fr, it, ja, tr, zh)
WEB_LANGUAGES = (site, ca, de, es, fr, it, ja, tr, zh)

LANGDICT = {l.code: l for l in LANGUAGES}

non_english_enabled_langs = [
  l for l in LANGUAGES if l.enabled and l.code != 'en'
]

# This file needs two modes of operations depending on
# wether it is run directly or imported.

if __name__ == '__main__':
    print(' '.join([l.code for l in LANGUAGES if l.enabled]))
else:
    translation_fallback = {l.code: (lambda x: x) for l in LANGUAGES}
    if 'LYDOC_LOCALEDIR' in os.environ:
        localedir = os.environ['LYDOC_LOCALEDIR']
        try:
            translation = {
              l.code: gettext.translation('lilypond-doc',
                                          localedir, [l.code]).gettext
              for l in non_english_enabled_langs
            }
        # TODO: use fallback=True in gettext.translation() ?
        except OSError:
            sys.stderr.write('langdefs.py: warning: lilypond-doc gettext '
                             'domain not found.\n')
            translation = translation_fallback

    else:
        translation = translation_fallback
