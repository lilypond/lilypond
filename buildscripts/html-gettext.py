#!@PYTHON@
# html-gettext.py

# USAGE:  html-gettext.py [-o OUTDIR] LANG FILES
#
# -o OUTDIR specifies that output files should be written in OUTDIR
#    rather than be overwritten
#

import sys
import re
import os
import getopt

import langdefs

optlist, args = getopt.getopt(sys.argv[1:],'o:')
lang = args[0]
files = args [1:]

outdir = '.'
for x in optlist:
    if x[0] == '-o':
        outdir = x[1]

double_punct_char_separator = langdefs.LANGDICT[lang].double_punct_char_sep
my_gettext = langdefs.translation[lang]

html_codes = ((' -- ', ' &ndash; '),
              (' --- ', ' &mdash; '),
              ("'", '&rsquo;'))
texi_html_conversion = {
    'command': {
        'html2texi':
            (re.compile (r'(?:<samp><span class="command">|<code>)(.*?)(?:</span></samp>|</code>)'),
             r'@command{\1}'),
        'texi2html':
            (re.compile (r'@command{(.*?)}'),
             r'<code>\1</code>'),
        },
    'code': {
        'html2texi':
            (re.compile (r'<code>(.*?)</code>'),
             r'@code{\1}'),
        'texi2html':
            (re.compile (r'@code{(.*?)}'),
             r'<code>\1</code>'),
        },
    }

whitespaces = re.compile (r'\s+')


def _ (s):
    if not s:
        return ''
    str = whitespaces.sub (' ', s)
    for c in html_codes:
        str = str.replace (c[1], c[0])
    for command in texi_html_conversion:
        d = texi_html_conversion[command]
        str = d['html2texi'][0].sub (d['html2texi'][1], str)
        str = my_gettext (str)
        str = d['texi2html'][0].sub (d['texi2html'][1], str)
    for c in html_codes:
        str = str.replace (c[0], c[1])
    return str

link_re =  re.compile (r'<link rel="(up|prev|next)" (.*?) title="([^"]*?)">')

def link_gettext (m):
    return '<link rel="' + m.group (1) + '" ' + m.group (2) \
        + ' title="' + _ (m.group (3)) + '">'

makeinfo_title_re = re.compile (r'<title>([^<]*?) - ([^<]*?)</title>')

def makeinfo_title_gettext (m):
    return '<title>' + _ (m.group (1)) + ' - ' + m.group (2) + '</title>'

texi2html_title_re = re.compile (r'<title>(.+): ([A-Z\d.]+ |)(.+?)</title>')

def texi2html_title_gettext (m):
    return '<title>' + _ (m.group (1)) + double_punct_char_separator + ': ' \
        + m.group (2) + _ (m.group (3)) + '</title>'

a_href_re = re.compile ('(?s)<a (?P<attributes>[^>]*?href="[\\w.#-_]+"[^>]*>)(?P<code><code>)?\
(?P<appendix>Appendix )?(?P<leading>[A-Z0-9.]+ | (?:&lt;){1,2} |&nbsp;[^:]+?:&nbsp;|&nbsp;|)\
(?P<name>.+?)(?P<end_code>(?(code)</code>|))(?P<trailing>| (?:&gt;){1,2} |&nbsp;|)</a>:?')

def a_href_gettext (m):
    s = ''
    if m.group(0)[-1] == ':':
        s = double_punct_char_separator + ':'
    t = ''
    if m.group ('appendix'):
        t = _ (m.group ('appendix'))
    return '<a ' + m.group ('attributes') + (m.group ('code') or '') + \
        t + m.group ('leading') + _ (m.group ('name')) + \
        m.group ('end_code') + m.group ('trailing') + '</a>' + s

h_re = re.compile (r'<h(\d)( class="\w+"|)>\s*(Appendix |)([A-Z\d.]+ |)(.+?)\s*</h\1>')

def h_gettext (m):
    if m.group (3):
        s = _ (m.group (3))
    else:
        s= ''
    return '<h' + m.group (1) + m.group (2) + '>' + s +\
           m.group (4) + _ (m.group (5)) + '</h' + m.group (1) + '>'

for filename in files:
    f = open (filename, 'r')
    page = f.read ()
    f.close ()
    page = link_re.sub (link_gettext, page)
    page = makeinfo_title_re.sub (makeinfo_title_gettext, page)
    page = texi2html_title_re.sub (texi2html_title_gettext, page)
    page = a_href_re.sub (a_href_gettext, page)
    page = h_re.sub (h_gettext, page)
    for w in ('Next:', 'Previous:', 'Up:'):
        page = page.replace (w, _ (w))
    page = langdefs.LANGDICT[lang].html_filter (page)
    f = open (os.path.join (outdir, filename), 'w')
    f.write (page)
    f.close ()
