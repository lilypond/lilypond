#!@PYTHON@
# lily-wins.py -- LilyPond command for .ly on Windows

import os
import re
import sys

def usage ():
	print 'Usage [-h,--help] lily-wins LY-FILE'

# print debugging stuff for now
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

def escape_shell (x):
 	return re.sub ("(\s|[`'\"\\\\])", r'\\\1',x)
#	return re.sub (r'''([^\\])([`'"\\\s])''', r'\1\\\2', x)
        # help emacs'" broken python mode
	
debug (`sys.argv`)

if len (sys.argv) != 2 \
   or sys.argv[1] == '-h' or sys.argv[1] == '--help':
	usage ()
	sys.exit (0)
	
native_file = sys.argv[1]
	
file = read_pipe ('/usr/bin/cygpath -au %s' % escape_shell (native_file))
if not file:
	file = native_file

cwd = os.getcwd ()

dir = os.path.dirname (file)
if not dir:
	dir = '.'
base = os.path.basename (file)
stem = strip_extension (base, '.ly')
print `vars ()`

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
if os.path.exists ('/usr/bin/ly2dvi'):
	system ('/usr/bin/ly2dvi -p %s > %s.log 2>&1' % (escape_shell (base),
							 escape_shell (stem)))
else:
	system ('/usr/bin/lilypond %s > %s.log 2>&1' % (escape_shell (base),
							escape_shell (stem)))
if not os.path.exists ('%(stem)s.pdf' % vars ()):
	# message box?
	sys.stderr.write ('pdf output not found\n')

system ('%s %s.pdf' % (escape_shell (pdfview), escape_shell (native_base)))
