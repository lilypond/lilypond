#!@PYTHON@
# lily-wins.py -- LilyPond command for .ly on Windows

import os
import re
import sys

def usage ():
	print 'Usage [-h,--help] lily-wins LY-FILE'

def debug (s):
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

debug (`sys.argv`)

if len (sys.argv) != 2 \
   or sys.argv[1] == '-h' or sys.argv[1] == '--help':
	usage ()
	sys.exit (0)
	
native_file = sys.argv[1]
	
file = read_pipe ("/usr/bin/cygpath -au '%(native_file)s'" % vars ())
if not file:
	file = native_file

cwd = os.getcwd ()

dir = os.path.dirname (file)
if not dir:
	dir = '.'
base = os.path.basename (file)
stem = strip_extension (base, '.ly')
print `vars ()`

native_base = read_pipe ('/usr/bin/cygpath -aw %(dir)s/%(stem)s' % vars ())
if not native_base:
	native_base = '%(dir)s/%(stem)s' % vars ()

pdfname = read_pipe ('/usr/bin/regtool get /root/.pdf/')
pdfopencommand = read_pipe ("/usr/bin/regtool get '/root/%(pdfname)s/shell/open/command/'" % vars ())

# hmm
native_view = re.sub ('"([^"]*).*"', '\\1', pdfopencommand)
if not native_view:
	native_view = 'acrobat'
	
if native_view:
	pdfview = read_pipe ("/usr/bin/cygpath -au '%(native_view)s'" % vars ())
if not pdfview:
	# message box?
	sys.stderr.write ('no pdf viewer found\n')
	pdfview = 'xpdf'

os.chdir (dir)
# avoid spaces in file
# system ('/usr/bin/ly2dvi -p %(file)s > %(stem)s.log 2>&1' % vars ())
system ('/usr/bin/ly2dvi -p %(base)s > %(stem)s.log 2>&1' % vars ()))
if not os.path.exists ('%(stem)s.pdf' % vars ()):
	# message box?
	sys.stderr.write ('pdf output not found\n')

system ("'%(pdfview)s' '%(native_base)s.pdf'" % vars ())
