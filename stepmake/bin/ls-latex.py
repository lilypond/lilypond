#!@PYTHON@

# name isn't really appropriate now ...

name = 'ls-latex'
version = '0.1'

import sys
import os
import string
import __main__
import glob
import re

format_names  = {'ps.gz': 'Compressed PostScript',
				 'html' : 'HTML'
				 }

def gulp_file(f):
	try:
		i = open(f)
		i.seek (0, 2)
		n = i.tell ()
		i.seek (0,0)
	except:
		sys.stderr.write ("can't open file: %s\n" % f)
		return ''
	s = i.read (n)
	if len (s) <= 0:
		sys.stderr.write ("gulped empty file: %s\n" % f)
	i.close ()
	return s

class Latex_head:
	def __init__ (self):
		self.author = ''
		self.title = ''
		self.date = ''
		self.format = ''
		
def read_latex_header (s):
	header = Latex_head()
	m = re.search(r'\\author{([^}]+)}', s)
	if m:
		header.author = m.group (1)

	m = re.search (r'\\title{([^}]+)}',s )
	if m:
		header.title = m.group (1)

	header.formats = ['ps.gz']
	return  header


def read_bib_header (s):
	m = re.search ('% *AUTHOR *= *(.*)\n',s)

	header = Latex_head()

	if m:
		header.author = m.group (1)

	m = re.search ('% *TITLE *= *(.*)\n',s )
	if m:
		header.title = m.group (1)

	header.formats = ['html']
	return header


def read_pod_header (s):
	header = Latex_head ()

	i = re.search( '[^\n \t]', s)
	s = s[i:]
	i = re.search( '\n\n', s)
	s = s[i+2:]	
	i = re.search( '\n\n', s)
	header.title = s[:i]

	header.formats = ['html']
	return  header

def read_texinfo_header (s):
	header = Latex_head ()

	m = re.search( '@settitle (.*)\n', s)
	if m:
		print 'title found! '
		header.title = m.group (1)
	m = re.search( '@author (.*)\n', s)
	if m:
		header.author = m.group (1)
	
	header.formats = ['html', 'ps.gz']
	return header

# urg
# should make a 'next_parens '
yo_article_re = re.compile ('article(\\([^)]*\\))[ \t\n]*(\\([^)]*\\))')
yo_report_re = re.compile ('report(\\([^)]*\\))[\t\n ]*(\\([^)]*\\))')
yo_sect_re  =  re.compile ('sect(\\([^)]*\\))')
yo_chapter_re  =  re.compile ('chapter(\\([^)]*\\))')

def read_yodl_header (s):
	header = Latex_head ()
	report = yo_report_re.search (s)
	article = 0
	sect = 0
	chapter = 0
	if report:
		header.author = report.group (2)
		header.title = yo_report_re.group (1)
	else:
		article = yo_article_re.search (s)
		if article:
			header.author = article.group (2)
			header.title = article.group (1)
		else:
			chapter = yo_chapter_re.search (s)
			if chapter:
				header.title = chapter.group (1)
			else:
				sect = yo_sect_re.search (s)
				if sect:
					header.title = sect.group (1)

	header.formats = ['html']
	return  header


def print_html_head (l,o,h):
	pre =o
	
	fn = pre + h.basename

	t = h.filename 
	if h.title :
		t = t + ': '+ h.title

	l.write ('<li>%s </a>' % t)

	if h.author:
		l.write ('<p>by %s</p>' % h.author)

	for f in h.formats:
		l.write ('(<a href=%s.%s>%s</a>)' % (fn, f, format_names [f]))
	l.write ('</li>\n')

def help ():
	sys.stdout.write (r"""Usage: ls-latex [OPTION]... FILE...
Generate html index file for FILE...

Options:
-h, --help                print this help
""")
	sys.exit (0)

import getopt

(options, files) = getopt.getopt(sys.argv[1:], 
	'e:h', ['help', 'prefix=',  'title='])

tex = ''
output =''
pre = ''
title = ''
for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--prefix':
		pre = a
	elif o == '--title':
		title = a  
	elif o == '-h' or o == '--help':
			help ()


l = sys.stdout

l.write (r"""<html><title>%s</title>
<body>
<h1> %s</h1><ul>
""" % (title, title))


read_header_funcs = {
	'pod' : read_pod_header,
	'tex' : read_latex_header,
	'doc' : read_latex_header,
	'bib': read_bib_header, 
	'latex' : read_latex_header,
	'tely' : read_texinfo_header,
	'texi': read_texinfo_header,
	'yo': read_yodl_header, 
}	


for x in files:
	m = re.search ('\\.([^.]*)$', x)
	if m == None:
		continue

	s = gulp_file (x)
	head = read_header_funcs [m.group(1)] (s)

	head.filename = x
	head.basename = re.sub ("\\.[^.]+", '', x)
	
	print_html_head (l, pre, head)

l.write ('</ul></body></html>')

