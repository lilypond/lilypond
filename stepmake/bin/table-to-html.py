#!@PYTHON@

# table-to-html.py -- convert char-separated table to html table
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

version = '0.1'
name = 'table-to-html'

import os
import sys
import getopt
import string
import time
import re

format = 'html'

def program_id ():
	return name + ' version ' + version;

def identify ():
	sys.stdout.write (program_id () + '\n')

def help ():
	sys.stdout.write (
r"""Usage: table-to-html [OPTION]... TABLE-FILE HTML-FILENAME
Generate pretty table from char separated table
Options:
  -h, --help			 print this help
  -p, --package=DIR	  specify package
  -s, --separator=SEP	specify separator [:]
  -t, --latex			do latex output instead
""")
	sys.exit (0)


def header (html):
	html.write ('<body bgcolor=white><table cellspacing=10>')

def footer (html):
	html.write ('</table></body>')

def convert_html (lines, outname, cols, separator, linesep):
	# ugh
	html = open (outname, 'w')

	header (html)
	i = 0
	for line in lines:
		i = i + 1
		if not len(line):
			continue
		columns = string.split (line, separator)
		html_line = '<tr><td>' + string.join (columns, '</td><td>') + '</td></tr>'
		html_line= re.sub (linesep, ' ', html_line)
		html.write (html_line)

		if len (columns) <> cols:
				print i
				raise 'not enough cols'

	footer (html)
	html.close ()


def convert_tex (lines, outname, cols, separator, linesep):
	html = open(outname, 'w')
	header = r"""\documentclass{article}
\begin{document}
{\parindent -1pc
	\parskip 0pc\parsep 0pc
	%  COMMENT( from the texbook)
	\def\length#1{\count0=0 \getlength#1\end}
	\def\getlength#1{\ifx#1\end \let\next=\relax
	  \else\advance\count0 by1 \let\next=\getlength\fi \next}
	  \def\inlanguage#1#2{{\length{#2}%
		\ifnum\count0=0
		\else
		\emph{#1}: #2.
		\fi}}
	\small

	\def\tableentry#1#2#3#4#5#6#7{\par{\bf #1}: #7
	  \inlanguage{Fran\c cais}{#2}
	   \inlanguage{British}{#4}  \inlanguage{Deutsch}{#3}
	   \inlanguage{Nederlands}{#5}\inlanguage{Italiano}{#6}}
"""

	html.write (header)
	i = 0
	for line in lines:
		i = i + 1
		if not len(line):
			continue
		columns = string.split (line, separator)
		if len (columns) <> cols:
				print i
				raise 'not enough cols'

		tex_line =  '\\tableentry{' + string.join (columns, '}{') + '}\n'
		tex_line = re.sub (linesep, ' ', tex_line)
		html.write (tex_line)
		
	html.write(r"""}\end{document}""")
	html.close ()

def convert_texinfo (lines, outname, cols, separator, linesep):
		pass



identify ()
(options, files) = getopt.getopt (
	sys.argv[1:], 'f:tl:o:h:c:s:', ['columns=', 'help', 'format=', 'output=', 'separator=', 'linesep='])
latex = 0
separator = '@'
output = ''
linesep = '\r'
for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--separator' or o == '-s':
		separator = a
	elif o== '--help' or o == '-h':
		help ()
	elif o=='--format' or o == '-f':
		format = a
	elif o == '--output' or o == '-o':
		output = a
	elif o == '--linesep' or o == '-l':
		linesep = a
	elif o == '--columns' or o == '-c':
		cols =  string.atoi(a)
	else:
		print o
		raise getopt.error

lines = open (files[0]).readlines ()

if format == 'latex':
	convert_tex (lines, output,  cols, separator, linesep)
elif format == 'html':
	convert_html (lines, output, cols, separator, linesep)
elif format == 'texi':
	convert_texinfo (lines, output, cols, separator, linesep)

