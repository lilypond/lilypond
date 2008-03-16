#!@PYTHON@

"""
USAGE: translations-status.py BUILDSCRIPT-DIR LOCALEDIR

  This script must be run from Documentation/

  Reads template files translations.template.html.in
and for each LANG in LANGUAGES LANG/translations.template.html.in

  Writes translations.html.in and for each LANG in LANGUAGES
translations.LANG.html.in
"""

import sys
import re
import string
import os
import gettext
import subprocess

def progress (str):
    sys.stderr.write (str + '\n')

progress ("translations-status.py")

buildscript_dir = sys.argv[1]
localedir = sys.argv[2]

_doc = lambda s: s

sys.path.append (buildscript_dir)
import langdefs

# load gettext messages catalogs
translation = {}
for l in langdefs.LANGUAGES:
    if l.enabled and l.code != 'en':
        translation[l.code] = gettext.translation('lilypond-doc', localedir, [l.code]).gettext

def read_pipe (command):
    child = subprocess.Popen (command,
                              stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE,
                              shell = True)
    (output, error) = child.communicate ()
    code = str (child.wait ())
    if not child.stdout or child.stdout.close ():
        print "pipe failed: %(command)s" % locals ()
    if code != '0':
        error = code + ' ' + error
    return (output, error)

comments_re = re.compile (r'^@ignore\n(.|\n)*?\n@end ignore$|@c .*?$', re.M)
space_re = re.compile (r'\s+', re.M)
lilypond_re = re.compile (r'@lilypond({.*?}|(.|\n)*?\n@end lilypond$)', re.M)
node_re = re.compile ('^@node .*?$', re.M)
title_re = re.compile ('^@(top|chapter|(?:sub){0,2}section|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?) (.*?)$', re.M)
include_re = re.compile ('^@include (.*?)$', re.M)

committish_re = re.compile ('GIT [Cc]ommittish: ([a-f0-9]+)')
translators_re = re.compile (r'^@c\s+Translators\s*:\s*(.*?)$', re.M | re.I)
checkers_re = re.compile (r'^@c\s+Translation\s*checkers\s*:\s*(.*?)$', re.M | re.I)
status_re = re.compile (r'^@c\s+Translation\s*status\s*:\s*(.*?)$', re.M | re.I)
post_gdp_re = re.compile ('post.GDP', re.I)
untranslated_node_str = 'UNTRANSLATED NODE: IGNORE ME'
skeleton_str = '-- SKELETON FILE --'

diff_cmd = 'git diff --no-color %(committish)s HEAD -- %(original)s | cat'

format_table = {
    'not translated': {'color':'d0f0f8', 'short':_doc ('no'), 'abbr':'NT',
                       'long':_doc ('not translated')},
    'partially translated': {'color':'dfef77', 'short':_doc ('partially (%(p)d %%)'),
                             'abbr':'%(p)d%%', 'long':_doc ('partially translated (%(p)d %%)')},
    'fully translated': {'color':'1fff1f', 'short':_doc ('yes'), 'abbr':'FT',
                         'long': _doc ('translated')},
    'up to date': {'short':_doc ('yes'), 'long':_doc ('up to date'), 'abbr':'100%%',
                   'vague':_doc ('up to date')},
    'outdated': {'short':_doc ('partially (%(p)d %%)'), 'abbr':'%(p)d%%',
                 'vague':_doc ('partially up to date')},
    'N/A': {'short':_doc ('N/A'), 'abbr':'N/A', 'color':'d587ff', 'vague':''},
    'pre-GDP':_doc ('pre-GDP'),
    'post-GDP':_doc ('post-GDP')
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
    'appendix': ('l', 1)
}

appendix_number_trans = string.maketrans ('@ABCDEFGHIJKLMNOPQRSTUVWXY',
                                          'ABCDEFGHIJKLMNOPQRSTUVWXYZ')

class SectionNumber (object):
    def __init__ (self):
        self.__data = [[0,'u']]

    def __increase_last_index (self):
        type = self.__data[-1][1]
        if type == 'l':
            self.__data[-1][0] = self.__data[-1][0].translate (appendix_number_trans)
        elif type == 'n':
            self.__data[-1][0] += 1

    def format (self):
        if self.__data[-1][1] == 'u':
            return ''
        return '.'.join ([str (i[0]) for i in self.__data if i[1] != 'u']) + ' '

    def increase (self, (type, level)):
        if level == 0:
            self.__data = [[0,'u']]
        while level + 1 < len (self.__data):
            del self.__data[-1]
        if level + 1 > len (self.__data):
            self.__data.append ([0, type])
            if type == 'l':
                self.__data[-1][0] = '@'
        if type == self.__data[-1][1]:
            self.__increase_last_index ()
        else:
            self.__data[-1] = ([0, type])
            if type == 'l':
                self.__data[-1][0] = 'A'
            elif type == 'n':
                self.__data[-1][0] = 1
        return self.format ()


def percentage_color (percent):
    p = percent / 100.0
    if p < 0.33:
        c = [hex (int (3 * p * b + (1 - 3 * p) * a))[2:] for (a, b) in [(0xff, 0xff), (0x5c, 0xa6), (0x5c, 0x4c)]]
    elif p < 0.67:
        c = [hex (int ((3 * p - 1) * b + (2 - 3 * p) * a))[2:] for (a, b) in [(0xff, 0xff), (0xa6, 0xff), (0x4c, 0x3d)]]
    else:
        c = [hex (int ((3 * p - 2) * b + 3 * (1 - p) * a))[2:] for (a, b) in [(0xff, 0x1f), (0xff, 0xff), (0x3d, 0x1f)]]
    return ''.join (c)

def tely_word_count (tely_doc):
    '''
    Calculate word count of a Texinfo document node by node.

    Take string tely_doc as an argument.
    Return a list of integers.

    Texinfo comments and @lilypond blocks are not included in word counts.
    '''
    tely_doc = comments_re.sub ('', tely_doc)
    tely_doc = lilypond_re.sub ('', tely_doc)
    nodes = node_re.split (tely_doc)
    return [len (space_re.split (n)) for n in nodes]


class TelyDocument (object):
    def __init__ (self, filename):
        self.filename = filename
        self.contents = open (filename).read ()

        ## record title and sectionning level of first Texinfo section
        m = title_re.search (self.contents)
        if m:
            self.title = m.group (2)
            self.level = texi_level [m.group (1)]
        else:
            self.title = 'Untitled'
            self.level = ('u', 1)

        included_files = [os.path.join (os.path.dirname (filename), t) for t in include_re.findall (self.contents)]
        self.included_files = [p for p in included_files if os.path.exists (p)]

    def print_title (self, section_number):
        return section_number.increase (self.level) + self.title


class TranslatedTelyDocument (TelyDocument):
    def __init__ (self, filename, masterdocument, parent_translation=None):
        TelyDocument.__init__ (self, filename)

        self.masterdocument = masterdocument

        ## record authoring information
        m = translators_re.search (self.contents)
        if m:
            self.translators = [n.strip () for n in m.group (1).split (',')]
        else:
            self.translators = parent_translation.translators
        m = checkers_re.search (self.contents)
        if m:
            self.checkers = [n.strip () for n in m.group (1).split (',')]
        elif isinstance (parent_translation, TranslatedTelyDocument):
            self.checkers = parent_translation.checkers
        else:
            self.checkers = []

        ## check whether translation is pre- or post-GDP
        m = status_re.search (self.contents)
        if m:
            self.post_gdp = bool (post_gdp_re.search (m.group (1)))
        else:
            self.post_gdp = False

        ## record which parts (nodes) of the file are actually translated
        self.partially_translated = not skeleton_str in self.contents
        nodes = node_re.split (self.contents)
        self.translated_nodes = [not untranslated_node_str in n for n in nodes]

        ## calculate translation percentage
        master_total_word_count = sum (masterdocument.word_count)
        translation_word_count = sum ([masterdocument.word_count[k] * self.translated_nodes[k]
                                       for k in range (min (len (masterdocument.word_count), len (self.translated_nodes)))])
        self.translation_percentage = 100 * translation_word_count / master_total_word_count

        ## calculate how much the file is outdated
        m = committish_re.search (self.contents)
        if not m:
            sys.stderr.write ('error: ' + filename + \
                                  ": no 'GIT committish: <hash>' found.\nPlease check " + \
                                  'the whole file against the original in English, then ' + \
                                  'fill in HEAD committish in the header.\n')
            sys.exit (1)
        (diff_string, error) = read_pipe (diff_cmd % {'committish':m.group (1), 'original':masterdocument.filename})
        if error:
            sys.stderr.write ('warning: %s: %s' % (self.filename, error))
            self.uptodate_percentage = None
        else:
            diff = diff_string.splitlines ()
            insertions = sum ([len (l) - 1 for l in diff if l.startswith ('+') and not l.startswith ('+++')])
            deletions = sum ([len (l) - 1 for l in diff if l.startswith ('-') and not l.startswith ('---')])
            outdateness_percentage = 50.0 * (deletions + insertions) / (masterdocument.size + 0.5 * (deletions - insertions))
            self.uptodate_percentage = 100 - int (outdateness_percentage)
            if self.uptodate_percentage > 100:
                alternative = 50
                progress ("%s: strange uptodateness percentage %d %%, setting to %d %%" \
                              % (self.filename, self.uptodate_percentage, alternative))
                self.uptodate_percentage = alternative
            elif self.uptodate_percentage < 1:
                alternative = 1
                progress ("%s: strange uptodateness percentage %d %%, setting to %d %%" \
                              % (self.filename, self.uptodate_percentage, alternative))
                self.uptodate_percentage = alternative

    def completeness (self, formats=['long']):
        if isinstance (formats, str):
            formats = [formats]
        p = self.translation_percentage
        if p == 0:
            status = 'not translated'
        elif p == 100:
            status = 'fully translated'
        else:
            status = 'partially translated'
        return dict ([(f, format_table[status][f] % locals()) for f in formats])

    def uptodateness (self, formats=['long']):
        if isinstance (formats, str):
            formats = [formats]
        p = self.uptodate_percentage
        if p == None:
            status = 'N/A'
        elif p == 100:
            status = 'up to date'
        else:
            status = 'outdated'
        l = {}
        for f in formats:
            if f == 'color' and p != None:
                l['color'] = percentage_color (p)
            else:
                l[f] = format_table[status][f] % locals ()
        return l

    def gdp_status (self, translation=lambda s: s):
        if self.post_gdp:
            return translation (format-table['post-GDP'])
        else:
            return translation (format-table['pre-GDP'])

    def short_html_status (self):
        s = '  <td>'
        if self.partially_translated:
            s += '<br>\n   '.join (self.translators) + '<br>\n'
            if self.checkers:
                s += '   <small>' + '<br>\n   '.join (self.checkers) + '</small><br>\n'

        c = self.completeness (['color', 'long'])
        s += '   <span style="background-color: #%(color)s">%(long)s</span><br>\n' % c

        if self.partially_translated:
            u = self.uptodateness (['vague', 'color'])
            s += '   <span style="background-color: #%(color)s">%(vague)s</span><br>\n' % u

        s += '  </td>\n'
        return s

    def text_status (self):
        s = self.completeness ('abbr')['abbr'] + ' '

        if self.partially_translated:
            s += self.uptodateness ('abbr')['abbr'] + ' '
        return s

    def html_status (self):
        # TODO
        return ''

class MasterTelyDocument (TelyDocument):
    def __init__ (self, filename, parent_translations=dict ([(lang, None) for lang in langdefs.LANGDICT.keys()])):
        TelyDocument.__init__ (self, filename)
        self.size = len (self.contents)
        self.word_count = tely_word_count (self.contents)
        translations = dict ([(lang, os.path.join (lang, filename)) for lang in langdefs.LANGDICT.keys()])
        self.translations = dict ([(lang, TranslatedTelyDocument (translations[lang], self, parent_translations.get (lang)))
                                   for lang in langdefs.LANGDICT.keys() if os.path.exists (translations[lang])])
        if self.translations:
            self.includes = [MasterTelyDocument (f, self.translations) for f in self.included_files]
        else:
            self.includes = []

    # TODO
    def print_wc_priority (self):
        return

    def html_status (self, numbering=SectionNumber ()):
        if self.title == 'Untitled' or not self.translations:
            return ''
        if self.level[1] == 0: # if self is a master document
            s = '''<table align="center" border="2">
 <tr align="center">
  <th>%s</th>''' % self.print_title (numbering)
            s += ''.join (['  <th>%s</th>\n' % l for l in self.translations.keys ()])
            s += ' </tr>\n'
            s += ' <tr align="left">\n  <td>Section titles<br>(%d)</td>\n' \
                % sum (self.word_count)

        else:
            s = ' <tr align="left">\n  <td>%s<br>(%d)</td>\n' \
                % (self.print_title (numbering), sum (self.word_count))

        s += ''.join ([t.short_html_status () for t in self.translations.values ()])
        s += ' </tr>\n'
        s += ''.join ([i.html_status (numbering) for i in self.includes])

        if self.level[1] == 0:
            s += '</table>\n<p></p>\n'
        return s

    def text_status (self, numbering=SectionNumber (), colspec=[48,12]):
        if self.title == 'Untitled' or not self.translations:
            return ''

        s = ''
        if self.level[1] == 0: # if self is a master document
            s += (self.print_title (numbering) + ' ').ljust (colspec[0])
            s += ''.join (['%s'.ljust (colspec[1]) % l for l in self.translations.keys ()])
            s += '\n'
            s += ('Section titles (%d)' % sum (self.word_count)).ljust (colspec[0])

        else:
            s = '%s (%d) ' \
                % (self.print_title (numbering), sum (self.word_count))
            s = s.ljust (colspec[0])

        s += ''.join ([t.text_status ().ljust(colspec[1]) for t in self.translations.values ()])
        s += '\n\n'
        s += ''.join ([i.text_status (numbering) for i in self.includes])

        if self.level[1] == 0:
            s += '\n'
        return s


progress ("Reading documents...")

tely_files = read_pipe ("find -maxdepth 2 -name '*.tely'")[0].splitlines ()
master_docs = [MasterTelyDocument (os.path.normpath (filename)) for filename in tely_files]
master_docs = [doc for doc in master_docs if doc.translations]

main_status_page = open ('translations.template.html.in').read ()

## TODO
#per_lang_status_pages = dict ([(l, open (os.path.join (l, 'translations.template.html')). read ())
#                               for l in langdefs.LANGDICT.keys ()
#                               if langdefs.LANGDICT[l].enabled])

progress ("Generating status pages...")

date_time = read_pipe ('LANG= date -u')[0]

main_status_html = ' <p><i>Last updated %s</i></p>\n' % date_time
main_status_html += '\n'.join ([doc.html_status () for doc in master_docs])

html_re = re.compile ('<html>', re.I)
end_body_re = re.compile ('</body>', re.I)

main_status_page = html_re.sub ('''<html>
<!-- This page is automatically generated by translation-status.py from
translations.template.html.in; DO NOT EDIT !-->''', main_status_page)

main_status_page = end_body_re.sub (main_status_html + '\n</body>', main_status_page)

open ('translations.html.in', 'w').write (main_status_page)

main_status_txt = '''Documentation translations status
Generated %s
NT = not translated
FT = fully translated

''' % date_time

main_status_txt += '\n'.join ([doc.text_status () for doc in master_docs])

status_txt_file = 'out/translations-status.txt'
progress ("Writing %s..." % status_txt_file)
open (status_txt_file, 'w').write (main_status_txt)
