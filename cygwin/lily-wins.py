#!@PYTHON@
# lily-wins.py -- LilyPond command for .ly on Windows

import getopt
import os
import re
import sys
import time

do_debug = 0

def usage ():
	print 'Usage [-h,--help] lily-wins LY-FILE'

# print debugging stuff for now
def debug (s):
	if do_debug:
		print s

def read_pipe (command):
	debug ('command:' + command)
	s = os.popen (command).read ()
	if s and s[-1] == '\n':
		return s[:-1]
	return s 

def system (command):
	debug ('command:' + command)
	os.system (command)

def strip_extension (f, ext):
	(p, e) = os.path.splitext (f)
	if e == ext:
		e = ''
	return p + e

def escape_shell (x):
 	return re.sub ("(\s|[`'\"\\\\])", r'\\\1',x)
#	return re.sub (r'''([^\\])([`'"\\\s])''', r'\1\\\2', x)
        # help emacs'" broken python mode

def usage ():
	print '''lily-wins [options] file

Options supported:

  -h, --help      this help screen
  -d, --debug     print debugging information
  
'''
	
debug (`sys.argv`)

########
# main
(opts, files)=getopt.getopt (sys.argv[1:],'dh', ['help', 'debug'])

for (o,a) in opts:
	if o == '-d' or o == '--debug':
		do_debug = 1
	elif o == '-h' or o  == '--help':
		usage ()
		sys.exit (0)

if files == []:
	usage ()
	sys.exit (1)
	
native_file = files[0]
print 'Processing %s ...' % native_file
file = read_pipe ('/usr/bin/cygpath -au %s' % escape_shell (native_file))
if not file:
	file = native_file

cwd = os.getcwd ()

dir = os.path.dirname (file)
if not dir:
	dir = '.'
base = os.path.basename (file)
stem = strip_extension (base, '.ly')
debug ( `vars ()`)

native_base = '%(dir)s/%(stem)s' % vars ()
native_base = read_pipe ('/usr/bin/cygpath -aw %s' % escape_shell (native_base))
			 
if not native_base:
	native_base = '%(dir)s/%(stem)s' % vars ()

pdfname = read_pipe ('/usr/bin/regtool get /root/.pdf/')
pdfopencommand = read_pipe ('/usr/bin/regtool get /root/%s/shell/open/command/' % escape_shell (pdfname))

# hmm
native_view = re.sub ('"([^"]*).*"', '\\1', pdfopencommand)
if not native_view:
	native_view = 'acrobat'
	
if native_view:
	pdfview = read_pipe ('/usr/bin/cygpath -au %s' % escape_shell (native_view))
if not pdfview:
	# message box?
	sys.stderr.write ('no pdf viewer found\n')
	pdfview = 'xpdf'

os.chdir (dir)
pdffile = '%(stem)s.pdf' % vars ()
if os.path.exists (pdffile):
	os.unlink (pdffile)


script = '/usr/bin/lilypond'

if os.path.exists ('/usr/bin/ly2dvi'):
	script = '/usr/bin/ly2dvi'

stat = system ('%s %s > %s.log 2>&1' % (script, escape_shell (base),
				    escape_shell (stem)))

if not os.path.exists (pdffile):
	# message box?
	sys.stderr.write ('PDF output not found. Error log: \n')

	map (sys.stderr.write, open (stem + '.log').readlines ()[-20:])
	sys.stderr.write ('A full log is in the file %s.log\n' % stem)
	sys.stderr.write ('\n\nPress enter to close window\n')
	sys.stdin.readline ()
else:
	
	# run even if failed, to make sure that error 
	system ('%s %s.pdf' % (escape_shell (pdfview), escape_shell (native_base)))
