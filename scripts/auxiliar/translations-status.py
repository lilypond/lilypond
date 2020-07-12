#!/usr/bin/env python
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2008--2020 John Mandereau <john.mandereau@gmail.com>
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


'''
USAGE: cd Documentation && translations-status.py

  Write:
    translations.itexi
    <LANG>/translations.itexi
    out/translations-status.txt

  Update word counts in:
    contributor/doc-translation-list.itexi

TODO:
   * using markup = TexiMarkup (), html tables (columns)
     are evenly spaced and bit too wide.  This can
     be fixed by using
        @multitable @columnfractions 0 0 0 0 0 0 0 0,
     but with that, PDF and info output get borked.
   * in info and PDF, columns have too little separation
   * using markup = HTMLMarkup (), we get nice
        <td title="FILENAME">
     popups -- do we want that with texi output? -- how?
     or possibly links to the git archive?

'''

import sys
import re
import operator
import os
#
import langdefs
import buildlib
from functools import reduce


def progress(str):
    sys.stderr.write(str + '\n')


exit_code = 0


def error(str, update_status=1):
    global exit_code
    sys.stderr.write('translations-status.py: %s\n' % str)
    exit_code = max(exit_code, update_status)


progress("translations-status.py")


def _doc(s): return s


# load gettext messages catalogs
translation = langdefs.translation


language_re = re.compile(r'^@documentlanguage (.+)', re.M)
comments_re = re.compile(r'^@ignore\n(.|\n)*?\n@end ignore$|@c .*?$', re.M)
space_re = re.compile(r'\s+', re.M)
lilypond_re = re.compile(r'@lilypond({.*?}|(.|\n)*?\n@end lilypond$)', re.M)
node_re = re.compile('^@node .*?$', re.M)
title_re = re.compile('^@(settitle|chapter|top|(?:sub){0,2}section|'
                      '(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?) (.*?)$', re.M)
include_re = re.compile('^@include (.*?)$', re.M)

# allow multiple lines
translators_re = re.compile(r'^@c[ ]+[Tt]ranslators?[ ]*:[ ]*(.*?)$', re.M)
checkers_re = re.compile(
    r'^@c[ ]+[Tt]ranslation[ ]*[Cc]heckers?[ ]*:[ ]*(.*?)$', re.M)
status_re = re.compile(
    r'^@c[ ]+[Tt]ranslation[ ]*[Ss]tatus[ ]*:[ ]*(.*?)$', re.M)
post_gdp_re = re.compile('post.GDP', re.I)
untranslated_node_str = '@untranslated'
skeleton_str = '-- SKELETON FILE --'

section_titles_string = _doc('Section titles')
last_updated_string = _doc('Last updated %s')
detailed_status_heads = [_doc('Translators'), _doc('Translation checkers'),
                         _doc('Translated'), _doc('Up to date'),
                         _doc('Other info')]
format_table = {
    'not translated': {'color': 'd0f0f8', 'short': _doc('no'), 'abbr': 'NT',
                       'long': _doc('not translated')},
    'partially translated': {'color': 'dfef77',
                             'short': _doc('partially (%(p)d %%)'),
                             'abbr': '%(p)d%%',
                             'long': _doc('partially translated (%(p)d %%)')},
    'fully translated': {'color': '1fff1f', 'short': _doc('yes'), 'abbr': 'FT',
                         'long': _doc('translated')},
    'up to date': {'short': _doc('yes'), 'long': _doc('up to date'),
                   'abbr': '100%%', 'vague': _doc('up to date')},
    'outdated': {'short': _doc('partially'), 'abbr': '%(p)d%%',
                 'vague': _doc('partially up to date')},
    'N/A': {'short': _doc('N/A'), 'abbr': 'N/A', 'color': 'd587ff', 'vague': ''},
    'pre-GDP': _doc('pre-GDP'),
    'post-GDP': _doc('post-GDP')
}

texi_level = {
    # (Unumbered/Numbered/Lettered, level)
    'top': ('u', 0),
    'unnumbered': ('u', 1),
    'unnumberedsec': ('u', 2),
    'unnumberedsubsec': ('u', 3),
    'chapter': ('n', 1),
    'section': ('n', 2),
    'subsection': ('n', 3),
    'appendix': ('l', 1),
    'appendixsec': ('l', 2),
}

appendix_number_trans = str.maketrans('@ABCDEFGHIJKLMNOPQRSTUVWXY',
                                      'ABCDEFGHIJKLMNOPQRSTUVWXYZ')


class SectionNumber (object):
    def __init__(self):
        self.__data = [[0, 'u']]

    def __increase_last_index(self):
        type = self.__data[-1][1]
        if type == 'l':
            self.__data[-1][0] = \
                self.__data[-1][0].translate(appendix_number_trans)
        elif type == 'n':
            self.__data[-1][0] += 1

    def format(self):
        if self.__data[-1][1] == 'u':
            return ''
        return '.'.join([str(i[0]) for i in self.__data if i[1] != 'u']) + ' '

    def increase(self, xxx_todo_changeme):
        (type, level) = xxx_todo_changeme
        if level == 0:
            self.__data = [[0, 'u']]
        while level + 1 < len(self.__data):
            del self.__data[-1]
        if level + 1 > len(self.__data):
            self.__data.append([0, type])
            if type == 'l':
                self.__data[-1][0] = '@'
        if type == self.__data[-1][1]:
            self.__increase_last_index()
        else:
            self.__data[-1] = ([0, type])
            if type == 'l':
                self.__data[-1][0] = 'A'
            elif type == 'n':
                self.__data[-1][0] = 1
        return self.format()


def percentage_color(percent):
    p = percent / 100.0
    if p < 0.33:
        c = [hex(int(3 * p * b + (1 - 3 * p) * a))[2:]
             for (a, b) in [(0xff, 0xff), (0x5c, 0xa6), (0x5c, 0x4c)]]
    elif p < 0.67:
        c = [hex(int((3 * p - 1) * b + (2 - 3 * p) * a))[2:]
             for (a, b) in [(0xff, 0xff), (0xa6, 0xff), (0x4c, 0x3d)]]
    else:
        c = [hex(int((3 * p - 2) * b + 3 * (1 - p) * a))[2:]
             for (a, b) in [(0xff, 0x1f), (0xff, 0xff), (0x3d, 0x1f)]]
    return ''.join(c)


def update_word_count(text, filename, word_count):
    return re.sub(r'(?m)^(\d+) *' + filename,
                  str(word_count).ljust(6) + filename,
                  text)


po_msgid_re = re.compile(r'^msgid "(.*?)"(?:\n"(.*?)")*', re.M)


def po_word_count(po_content):
    s = ' '.join([''.join(t) for t in po_msgid_re.findall(po_content)])
    return len(space_re.split(s))


sgml_tag_re = re.compile(r'<.*?>', re.S)


def sgml_word_count(sgml_doc):
    s = sgml_tag_re.sub('', sgml_doc)
    return len(space_re.split(s))


def tely_word_count(tely_doc):
    '''
    Calculate word count of a Texinfo document node by node.

    Take string tely_doc as an argument.
    Return a list of integers.

    Texinfo comments and @lilypond blocks are not included in word counts.
    '''
    tely_doc = comments_re.sub('', tely_doc)
    tely_doc = lilypond_re.sub('', tely_doc)
    nodes = node_re.split(tely_doc)
    return [len(space_re.split(n)) for n in nodes]


class HTMLMarkup (object):
    texi_header = '''@c -*- coding: utf-8; mode: texinfo; -*-
@c This file was generated by translation-status.py -- DO NOT EDIT!
@ignore
    Translation of GIT committish: 0
@end ignore

'''
    texi_footer = '''
'''

    def texi(self, string):
        return (self.texi_header
                + '''
@ifnothtml
Translation status currently only available in HTML.
@end ifnothtml
'''
                + string
                + self.texi_footer)

    def entity(self, name, string='', attributes=[]):
        attr_list = ''.join([' %s="%s"' % x for x in attributes])
        return '<%(name)s%(attr_list)s>%(string)s</%(name)s>' % locals()

    def paragraph(self, string=''):
        return self.entity('p', string)

    def table(self, string):
        return self.entity('table', string, [('align', 'center'), ('border', '2')])

    def row(self, string, attributes=[]):
        return self.entity('tr', string, attributes)
    headrow = row

    def headcell(self, string, attributes=[]):
        return self.entity('th', string, attributes)

    def cell(self, string='', attributes=[]):
        return self.entity('td', string, attributes)

    def newline(self, attributes=[]):
        return self.entity('br', '', attributes)[:-5]

    def span(self, string, attributes=[]):
        return self.entity('span', string, attributes)

    def small(self, string, attributes=[]):
        return self.entity('small', string, attributes)

    def emph(self, string, attributes=[]):
        return self.entity('em', string, attributes)


class TexiMarkup (HTMLMarkup):
    def texi(self, string):
        return (self.texi_header
                + self.html('''
<style type="text/css"><!--
th { border: 1px solid black; text-align: center; }
td { border: 1px solid black; text-align: center; }
!--></style>
''')
                + self.columnfraction_disaster(self.itemtab_disaster(string))
                + self.texi_footer)

    def itemtab_disaster(self, string):
        return string.replace('''item \n@tab ''', '''item
''')

    def columnfraction_disaster(self, string):
        if False:
            # nice trick for html-only
            return string.replace('@multitable', '@multitable @columnfractions 0 0 0 0 0 0 0 0 0 0')
        tables = re.findall('(?s)(@multitable)(.*?)(@item)', string)
        for t in tables:
            columns = len(re.findall('(?s)(\n@tab)', t[1])) + 1
            columnfractions = '@columnfractions ' + \
                (' ' + str(1.0/columns)) * columns
            string = string.replace('@multitable\n',
                                    '@multitable %(columnfractions)s\n' % locals(), 1)
        return string

    def entity(self, name, string='', attributes=[]):
        return '''
@%(name)s
%(string)s
@end %(name)s''' % locals()

    def paragraph(self, string=''):
        return '''
%(string)s''' % locals()

    def table(self, string):
        # Ugh, makeinfo is fine without @columnfractions
        # but texi2html 1.82 barfs: `empty multicolumn'
        return (self.entity('multitable', string))

    def headrow(self, string, attributes=[]):
        return '''
@headitem ''' + string

    def row(self, string, attributes=[]):
        return '''
@item ''' + string

    def cell(self, string='', attributes=[]):
        return '''
@tab ''' + string
    headcell = cell

    def newline(self):
        return '''
@*
'''

    def html(self, string):
        return self.entity('ifhtml', self.entity('html', string))

    def nothtml(self, string):
        return self.entity('ifnothtml', string)

    def span(self, string, attributes=[]):
        return (self.html(HTMLMarkup().span(string, attributes))
                + self.nothtml(string))

    def small(self, string, attributes=[]):
        return (self.html(HTMLMarkup().small(string, attributes))
                + self.nothtml(string))

    def command(self, name, string):
        return '@%(name)s{%(string)s}' % locals()

    def emph(self, string, attributes=[]):
        return self.command('emph', string)


class TelyDocument (object):
    def __init__(self, filename):
        self.filename = filename
        self.contents = 'GIT committish: 0'
        if os.path.exists(filename):
            self.contents = open(filename).read()
        # record title and sectionning level of first Texinfo section
        self.sectioning = 'unnumbered'
        self.title = 'Untitled'
        m = title_re.search(self.contents)
        if m:
            self.sectioning = m.group(1)
            self.title = m.group(2)

        if not hasattr(self, 'language'):
            self.language = ''
        m = language_re.search(self.contents)
        if m:
            self.language = m.group(1)

        dir = os.path.dirname(filename).split('/')[0]
        if len(dir) == 2:
            dir += '/'
        else:
            dir = ''
        included_files = [dir + t
                          for t in include_re.findall(self.contents)]
        self.included_files = [p for p in included_files if os.path.exists(p)]

    def get_level(self):
        return texi_level[self.sectioning]

    def print_title(self, section_number):
        if not hasattr(self, 'level'):
            self.level = self.get_level()
        return section_number.increase(self.level) + self.title


class TranslatedTelyDocument (TelyDocument):
    def __init__(self, filename, masterdocument, parent_translation=None):
        TelyDocument.__init__(self, filename)
        self.masterdocument = masterdocument
        if not hasattr(self, 'language'):
            self.language = ''
        if not self.language and parent_translation:
            self.language = parent_translation.__dict__.get('language', '')
        if self.language == 'en':
            print(
                filename + ': language en specified: set @documentlanguage', self.filename[:2])
            self.language = ''
        if not self.language and filename[2] == '/':
            print(
                filename + ': no language specified: add @documentlanguage', self.filename[:2])
            self.language = filename[:2]
        if self.language:
            self.translation = translation[self.language]
        else:
            self.translation = lambda x: x
        self.title = self.translation(self.title)

        # record authoring information
        self.translators = ['']
        if parent_translation:
            self.translators = parent_translation.__dict__.get(
                'translators', [''])
        m = translators_re.findall(self.contents)
        if m:
            self.translators = [n.strip() for n in
                                reduce(operator.add, [n.split(',') for n in m])]
        if self.language != self.filename[:2]:
            print('Barf:', self.filename)
            barf
        if (not isinstance(self, UntranslatedTelyDocument)
            and (not self.translators or not self.translators[0])
                and not 'macros.itexi' in self.filename):
            error(self.filename + ''': error: no translator name found
    please specify one ore more lines in the master file
    @c Translator: FirstName LastName[, FirstName LastName]..''')
        self.checkers = []
        m = checkers_re.findall(self.contents)
        if m:
            self.checkers = [n.strip() for n in
                             reduce(operator.add, [n.split(',') for n in m])]
        if not self.checkers and isinstance(parent_translation, TranslatedTelyDocument):
            self.checkers = parent_translation.checkers

        # check whether translation is pre- or post-GDP
        m = status_re.search(self.contents)
        if m:
            self.post_gdp = bool(post_gdp_re.search(m.group(1)))
        else:
            self.post_gdp = False

        # record which parts (nodes) of the file are actually translated
        self.partially_translated = not skeleton_str in self.contents
        nodes = node_re.split(self.contents)
        self.translated_nodes = [not untranslated_node_str in n for n in nodes]

        # calculate translation percentage
        master_total_word_count = sum(masterdocument.word_count)
        translation_word_count = \
            sum([masterdocument.word_count[k] * self.translated_nodes[k]
                 for k in range(min(len(masterdocument.word_count),
                                    len(self.translated_nodes)))])
        self.translation_percentage = \
            100 * translation_word_count / master_total_word_count

        # calculate how much the file is outdated
        (diff_string, git_error) = \
            buildlib.check_translated_doc(
                masterdocument.filename, self.filename, self.contents)
        if git_error:
            sys.stderr.write('warning: %s: %s' % (self.filename, git_error))
            self.uptodate_percentage = None
        else:
            diff = diff_string.splitlines()
            insertions = sum([len(l) - 1 for l in diff
                              if l.startswith('+')
                              and not l.startswith('+++')])
            deletions = sum([len(l) - 1 for l in diff
                             if l.startswith('-')
                             and not l.startswith('---')])
            outdateness_percentage = 50.0 * (deletions + insertions) / \
                (masterdocument.size + 0.5 * (deletions - insertions))
            self.uptodate_percentage = 100 - int(outdateness_percentage)
            if self.uptodate_percentage > 100:
                alternative = 50
                progress("%s: strange uptodateness percentage %d %%, \
setting to %d %%" % (self.filename, self.uptodate_percentage, alternative))
                self.uptodate_percentage = alternative
            elif self.uptodate_percentage < 1:
                alternative = 1
                progress("%s: strange uptodateness percentage %d %%, \
setting to %d %%" % (self.filename, self.uptodate_percentage, alternative))
                self.uptodate_percentage = alternative

    def get_level(self):
        return texi_level['top']

    def completeness(self, formats=['long'], translated=False):
        if translated:
            translation = self.translation
        else:
            def translation(x): return x

        if isinstance(formats, str):
            formats = [formats]
        p = self.translation_percentage
        if p == 0:
            status = 'not translated'
        elif p == 100:
            status = 'fully translated'
        else:
            status = 'partially translated'
        # Call locals() outside of the list comprehension to get the local
        # values of the function!
        context = locals()
        return dict([(f, translation(format_table[status][f]) % context)
                     for f in formats])

    def uptodateness(self, formats=['long'], translated=False):
        if translated:
            translation = self.translation
        else:
            def translation(x): return x

        if isinstance(formats, str):
            formats = [formats]
        p = self.uptodate_percentage
        if p is None:
            status = 'N/A'
        elif p == 100:
            status = 'up to date'
        else:
            status = 'outdated'
        l = {}
        for f in formats:
            if f == 'color' and p is not None:
                l['color'] = percentage_color(p)
            else:
                l[f] = translation(format_table[status][f]) % locals()
        return l

    def gdp_status(self):
        if self.post_gdp:
            return self.translation(format_table['post-GDP'])
        else:
            return self.translation(format_table['pre-GDP'])

    def short_texi_status(self, markup):
        s = ''
        if self.partially_translated:
            s += markup.newline().join(self.translators + [''])
            if self.checkers:
                s += markup.newline().join([markup.small(x)
                                            for x in self.checkers + ['']])
        c = self.completeness(['color', 'long'])
        s += markup.span('%(long)s' %
                         c, [('style', 'background-color: #%(color)s' % c)])
        s += markup.newline()
        if self.partially_translated:
            u = self.uptodateness(['vague', 'color'])
            s += markup.span('%(vague)s' %
                             u, [('style', 'background-color: #%(color)s' % u)])
        return markup.cell(s, [('title', self.filename)])

    def text_status(self):
        s = self.completeness('abbr')['abbr'] + ' '
        if self.partially_translated:
            s += self.uptodateness('abbr')['abbr'] + ' '
        return s

    def texi_status(self, markup, numbering=SectionNumber()):
        return (markup.table(
                markup.headrow(
                    (markup.headcell(self.print_title(numbering))
                     + ''.join([markup.headcell(self.translation(h))
                                for h in detailed_status_heads])),
                    [('align', 'center')])
                + markup.row(
                    (markup.cell((self.translation(section_titles_string)
                                  + markup.newline()
                                  + '%d' % sum(self.masterdocument.word_count)),
                                 [('title', self.filename)])
                     + self.texi_body(markup, numbering)),
                    [('align', 'left')])
                + self.texi_translations(markup, numbering))
                ) + markup.paragraph()

    def texi_body(self, markup, numbering):
        return (self.texi_translators(markup)
                + self.texi_completeness(markup)
                + self.texi_uptodateness(markup)
                + self.texi_gdp(markup))

    def texi_translators(self, markup):
        if self.partially_translated:
            return (markup.cell(markup.newline().join(self.translators))
                    + markup.cell(markup.newline().join(self.checkers)))
        return markup.cell() + markup.cell()

    def texi_completeness(self, markup):
        c = self.completeness(['color', 'short'], translated=True)
        return markup.cell(markup.span(c['short'],
                                       [('style', 'background-color: #' + c['color'])]))

    def texi_uptodateness(self, markup):
        if self.partially_translated:
            u = self.uptodateness(['short', 'color'], translated=True)
            return markup.cell(markup.span(u['short'],
                                           [('style', 'background-color: #' + u['color'])]))
        return markup.cell()

    def texi_gdp(self, markup):
        return markup.cell(self.gdp_status())

    def texi_translations(self, markup, numbering):
        return ''.join([i.translations[self.language].texi_status(markup, numbering)
                        for i in self.masterdocument.includes
                        if self.language in i.translations])


class IncludedTranslatedTelyDocument (TranslatedTelyDocument):
    get_level = TelyDocument.get_level

    def texi_status(self, markup, numbering=SectionNumber()):
        if self.title != 'Untitled':
            return (markup.row(
                    (markup.cell((
                        self.print_title(numbering)
                        + markup.newline()
                        + '%d' % sum(self.masterdocument.word_count)),
                        [('title', self.filename)])
                     + self.texi_body(markup, numbering)),
                    [('align', 'left')])
                    + self.texi_translations(markup, numbering))
        return ''


class UntranslatedTelyDocument (TranslatedTelyDocument):
    def __init__(self, filename, masterdocument, parent_translation=None):
        if filename[2] == '/':
            self.language = filename[:2]
        TranslatedTelyDocument.__init__(
            self, filename, masterdocument, parent_translation)


class IncludedUntranslatedTelyDocument (UntranslatedTelyDocument, IncludedTranslatedTelyDocument):
    get_level = TelyDocument.get_level


class MasterTelyDocument (TelyDocument):
    def __init__(self,
                 filename,
                 parent_translations=dict([(lang, None)
                                           for lang in langdefs.LANGDICT])):
        TelyDocument.__init__(self, filename)
        self.size = len(self.contents)
        self.word_count = tely_word_count(self.contents)
        self.translations = {}
        self.includes = []
        if not self.language or self.language == 'en':
            languages = [x for x in list(
                parent_translations.keys()) if x != 'en']
            self.translations = dict([x for x in
                                      [(lang, self.translated_factory(os.path.join(lang, self.filename),
                                                                      parent_translations.get(lang)))
                                       for lang in languages]
                                      if x[1]])
            if self.translations:
                self.includes = [IncludedMasterTelyDocument(f, self.translations)
                                 for f in self.included_files]

    def get_level(self):
        return texi_level['top']

    def translated_factory(self, filename, parent):
        if os.path.exists(filename):
            return TranslatedTelyDocument(filename, self, parent)
        return None

    def update_word_counts(self, s):
        s = update_word_count(s, self.filename, sum(self.word_count))
        for i in self.includes:
            s = i.update_word_counts(s)
        return s

    def texi_status(self, markup, numbering=SectionNumber()):
        return markup.table(
            (markup.headrow(
                (markup.headcell(self.print_title(numbering))
                 + ''.join([markup.headcell(l) for l in sorted(self.translations.keys())])),
                [('align', 'center')])
             + markup.row(
                (markup.cell(('Section titles'
                              + markup.newline()
                              + '(%d)' % sum(self.word_count)),
                             [('title', self.filename)])
                 + self.texi_body(markup, numbering)),
                [('align', 'left')])
             + self.texi_includes(markup, numbering)
             )) + markup.paragraph()

    def texi_includes(self, markup, numbering):
        return ''.join([i.texi_status(markup, numbering) for i in self.includes])

    def texi_body(self, markup, numbering):
        return ''.join([self.translations[k].short_texi_status(markup)
                        for k in sorted(self.translations.keys())])

    def text_status(self, markup, numbering=SectionNumber(), colspec=[48, 12]):
        s = (self.print_title(numbering) + ' ').ljust(colspec[0])
        s += ''.join(['%s'.ljust(colspec[1]) % l
                      for l in sorted(self.translations.keys())])
        s += '\n'
        s += ('Section titles (%d)' %
              sum(self.word_count)).ljust(colspec[0])
        s += self.text_body(markup, numbering, colspec)
        s += '\n'
        return s

    def text_body(self, markup, numbering, colspec):
        return (''.join([self.translations[k].text_status().ljust(colspec[1])
                         for k in sorted(self.translations.keys())])
                + '\n\n'
                + ''.join([i.text_status(markup, numbering) for i in self.includes]))


class IncludedMasterTelyDocument (MasterTelyDocument):
    get_level = TelyDocument.get_level

    def translated_factory(self, filename, parent):
        if os.path.exists(filename):
            return IncludedTranslatedTelyDocument(filename, self, parent)
        return IncludedUntranslatedTelyDocument(filename, self, parent)

    def texi_status(self, markup, numbering=SectionNumber()):
        if self.title != 'Untitled':
            return (markup.row(
                    (markup.cell((self.print_title(numbering)
                                  + markup.newline()
                                  + '(%d)' % sum(self.word_count)),
                                 [('title', self.filename)])
                     + self.texi_body(markup, numbering)),
                    [('align', 'left')])
                    + self.texi_includes(markup, numbering))
        return ''

    def text_status(self, markup, numbering=SectionNumber(), colspec=[48, 12]):
        if self.title != 'Untitled':
            return (self.print_title(numbering)
                    + '(%d)' % sum(self.word_count)
                    + self.text_body(markup, numbering, colspec)
                    ).ljust(colspec[0])
        return ''


update_category_word_counts_re = re.compile(r'(?ms)^-(\d+)-(.*?\n)\d+ *total')

counts_re = re.compile(r'(?m)^(\d+) ')


def update_category_word_counts_sub(m):
    return ('-' + m.group(1) + '-' + m.group(2)
            + str(sum([int(c)
                       for c in counts_re.findall(m.group(2))])).ljust(6)
            + 'total')

# urg
# main () starts here-abouts


progress("Reading documents...")

master_files = \
    buildlib.read_pipe(
        "git ls-files | grep -E '[^/]*/?[^/]*[.](tely|texi)$'")[0].splitlines()
master_files.sort()
master_docs = [MasterTelyDocument(os.path.normpath(filename))
               for filename in master_files]
master_docs = [doc for doc in master_docs if doc.translations]

enabled_languages = [l for l in langdefs.LANGDICT
                     if langdefs.LANGDICT[l].enabled
                     and l != 'en']

progress("Generating status pages...")

date_time = buildlib.read_pipe('LANG= date -u')[0]

# TEXI output sort of works
# TODO: table border, td-titles :-)
# markup = HTMLMarkup ()
#sys.stderr.write ('''translations-status.py:713: warning: using markup = HTMLMarkup (): HTML only\n''')
markup = TexiMarkup()
sys.stderr.write('''translations-status.py:717: warning: using markup = TexiMarkup (): ugly HTML
    output, questionable PDF and info output.
    Consider using HTML-only markup = HTMLMarkup ()\n''')

main_status_body = markup.paragraph(
    markup.emph(last_updated_string % date_time))
main_status_body += '\n'.join([doc.texi_status(markup) for doc in master_docs])
main_status_page = markup.texi(main_status_body)

open('translations.itexi', 'w').write(main_status_page)

for l in enabled_languages:
    date_time = buildlib.read_pipe('LANG=%s date -u' % l)[0]
    updated = markup.paragraph(markup.emph(
        translation[l](last_updated_string) % date_time))
    texi_status = '\n'.join([doc.translations[l].texi_status(markup)
                             for doc in master_docs
                             if l in doc.translations])
    lang_status_page = markup.texi(updated + texi_status)
    open(os.path.join(l, 'translations.itexi'), 'w').write(lang_status_page)

main_status_txt = '''Documentation translations status
Generated %s
NT = not translated
FT = fully translated

''' % date_time

main_status_txt += '\n'.join([doc.text_status(markup) for doc in master_docs])

status_txt_file = 'out/translations-status.txt'
progress("Writing %s..." % status_txt_file)
open(status_txt_file, 'w').write(main_status_txt)

translation_instructions_file = 'contributor/doc-translation-list.itexi'
progress("Updating %s..." % translation_instructions_file)
translation_instructions = open(translation_instructions_file).read()

for doc in master_docs:
    translation_instructions = doc.update_word_counts(translation_instructions)

for html_file in re.findall(r'(?m)^\d+ *(\S+?\.html\S*?)(?: |$)',
                            translation_instructions):
    word_count = sgml_word_count(open(html_file).read())
    translation_instructions = update_word_count(translation_instructions,
                                                 html_file,
                                                 word_count)

for po_file in re.findall(r'(?m)^\d+ *(\S+?\.po\S*?)(?: |$)',
                          translation_instructions):
    word_count = po_word_count(open(po_file).read())
    translation_instructions = update_word_count(translation_instructions,
                                                 po_file,
                                                 word_count)

translation_instructions = \
    update_category_word_counts_re.sub(update_category_word_counts_sub,
                                       translation_instructions)

open(translation_instructions_file, 'w').write(translation_instructions)
sys.exit(exit_code)
