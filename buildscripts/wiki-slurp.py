#!@PYTHON@

#
# translate an entire Wiki site into local html.
#

import re
import urllib
import sys
import getopt
import os
program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.5.69'


def help ():
	print """Usage: wiki-slurp.py [OPTION]... ENTRY-PATTERN...

Download a WikiWiki site and convert to local html.

Example: wiki-slurp.py -d /tmp/output 'http://c2.com/cgi-bin/wiki?'

Options:
  -h,--help          this help
  -m,--mangle        mangle file names to be shorter 
  -d,--outdir=DIR    set output directory to DIR
  -v,--version       version information

Warning: downloading an entire Wiki site generates a huge amount of
traffic and server load.  Consider asking for a copy of the database.
See also http://c2.com/cgi-bin/wiki?WikiSlurp

Report bugs to bug-lilypond@gnu.org.

Written by Han-Wen Nienhuys <hanwen@cs.uu.nl>
"""

def print_version ():
	print r"""wiki-slurp.py %s

This is free software.  It is covered by the GNU General Public License,
and you are welcome to change it and/or distribute copies of it under
certain conditions.  Invoke as `midi2ly --warranty' for more information.

Copyright (c) 2000-2002 by Han-Wen Nienhuys <hanwen@cs.uu.nl>
""" % program_version

(options, files) = getopt.getopt (sys.argv[1:], 'vd:hm', ['help','mangle','version', 'outdir='])


def identity (name):
	return name

def mangle (name):
	return '%d' % hash (name)

mangler = identity

outdir = '/tmp/'

for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
		sys.exit (0)
	elif o == '--version' or o == '-v':
		print_version ()
		sys.exit(0)
	elif  o == '--mangle' or o == '-m':
		mangler = mangle
	elif o == '--outdir' or o == '-d':
		outdir = a
	else:
		print o
		raise getopt.error


patterns = files

if not patterns:
	help ()
	sys.stderr.write ("\n")
	sys.exit (2)

re_patterns = []
for pattern in patterns:
	re_patterns.append (re.sub ('([?.])', '\\\\\\1', pattern))

todo = ["FrontPage"]

print 'here!'
done = {
	'EditText': 1,  
	}

def unwiki (str, pat, mangler):
	local = '<a href="%s([A-Za-z]+)">([A-Za-z]+)</a>' % pat

	newurls = []
	def do_replace (match, us = newurls, mangler = mangler):
		newurl = match.group (1)
		local = mangler (newurl)
		
		replacement = '<a href="%s.html">%s</a>' % (local,newurl)
		us.append (newurl)
		return replacement

	str = re.sub (local, do_replace, str)
	otherurl = '<a href="%s[^>]*">([?A-Za-z]+)</a>' % pat
	str = re.sub (otherurl, '\\1', str)

	imagesrc = '<a href="%s[^>]*">(<img[^>]*>)</a>' % pat
	str = re.sub (imagesrc, '\\1', str)
	
	return (str, newurls)


while todo:
	f = todo[-1]
	todo = todo[:-1]

	if done.has_key (f):
		continue
	done [f] = 1

	mangled = mangler (f)
	
	sys.stderr.write ("reading `%s' ... " % f)
	sys.stderr.flush ()

	page = urllib.urlopen (patterns[0] + f).read ()
	sys.stderr.write ('done. ')
	sys.stderr.flush ()

	for re_pattern in re_patterns:
		(page, nus) = unwiki (page, re_pattern, mangler)
		todo.extend (nus)
	
	outname = os.path.join (outdir, mangled) + '.html'
	fo = open (outname, 'w')

	sys.stderr.write ("Writing `%s'\n" % outname)
	fo.write (page)
	fo.close ()


# test
if 0:
	page = open ('/tmp/FrontPage.html').read()
	(str, us)=unwiki (page, re_patterns)
	print str
	print us

