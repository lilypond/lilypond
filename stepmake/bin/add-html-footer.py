#!@PYTHON@

"""
Print a nice footer.  add the top of the ChangeLog file (up to the ********)
"""

program_name = 'add-html-footer'
version = '0.1'

import sys
import os
import time
import string 
import getopt
import __main__

fullname = "unknown"
changelog_file = ''

index_file=''
banner_file = ''
changelog_file=''
changes =''
footer = '\n<hr>Please take me <a href=%s>back to the index</a>\n\
of %s<!%s%s>\n'
builtstr = '\n<hr><font size=-1>\n\
This page was built from %s-%s by\
<address><br>%s &lt<a href=mailto:%s>%s</a>&gt, at %s.</address><p></font>' 

(options, files) = getopt.getopt(sys.argv[1:], 'c:hp:', ['changelog=', 'help', 'news=', 'index=', 'package=']) 

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
		sys.stderr.write ("gulped emty file: %s\n" % f)
	i.close ()
	return s

def help ():
    sys.stdout.write (r"""Usage: add-html-footer [OPTION]... HTML-FILE
Add a nice footer, add the top of the ChangLog file (up to the ********)
Options:
  -h, --help             print this help
  -p, --package          package name (ugh. Junkme.)
  """)
    sys.exit (0)

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--news' or o == '--changelog' or o == '-c':
	changelog_file = a
    elif o == '--index':
	index_file = a
    elif o == '-h' or o == '--help':
    	help ()
    elif o == '-p' or o == '--package':
	topdir = a

sys.path.append (topdir + '/stepmake/bin')
from packagepython import *
package = Package (topdir)
packager = Packager ()

if package.NAME == 'LILYPOND':
    package.Name = 'GNU LilyPond'

def set_vars():
    os.environ["CONFIGSUFFIX"] = 'www';
    if os.name == 'nt':
        import ntpwd
        pw = ntpwd.getpwname(os.environ['USERNAME'])
    else:
        import pwd
        pw = pwd.getpwuid (os.getuid());

    __main__.fullname=pw[4]

set_vars ()

def footstr(index):
    try:
    	footer = gulp_file (package.topdir + '/Documentation/footer.html.in')
    except:
        pass
    s = footer % (index, package.Name, packager.webmaster, packager.webmaster)
    s = s + builtstr % (package.Name, 
    			version_tuple_to_str (package.version), fullname,
		        packager.mail, packager.mail, 
			time.strftime ('%c %Z', time.localtime (time.time ())))
    return s

banner = footstr (index_file)
banner_id = '<! banner_id >'


if changelog_file:
    changes = gulp_file (changelog_file)
    i = regex.search ('^\*\*', changes)
    changes = changes[:i]
    
def check_tag (tag, sub, s, bottom):
    tag = string.lower (tag)
    TAG = string.upper (tag)
    s = regsub.sub (tag, TAG, s)
    i = regex.search (TAG, s)
    if i < 0:
        if bottom:
	    s = s + sub + '\n'
	else:
	    s = sub + '\n' + s
    return s
    
for f in files:
    s = gulp_file (f)

    if changelog_file:
	s = regsub.sub ('top_of_ChangeLog', '<XMP>\n'+ changes  + '\n</XMP>\n', s)

    s = check_tag ('<body', '', s, 0)
    if regex.search ('<BODY', s) == -1:
	s = '<BODY>\n' + s
    s = regsub.sub ('<BODY>', '<BODY BGCOLOR=WHITE><FONT COLOR=BLACK>', s)
    if regex.search (banner_id, s) == -1:
	s = regsub.sub ('</body>', '</BODY>', s)
	s = regsub.sub ('</BODY>', banner_id  + banner + '</BODY>', s)
    else:
	s = check_tag ('</body>', '</BODY>', s, 1)

    title = '<HEAD><TITLE>' \
	+ package.Name + ' -- ' + os.path.basename (os.path.splitext(f)[0]) \
	+ '</TITLE></HEAD>'
    s = check_tag ('<title>', title, s, 0)

    s = check_tag ('<html', '', s, 0)
    if regex.search ('<HTML', s) == -1:
	s = '<HTML>\n' + s
    s = check_tag ('</html>', '</HTML>', s, 1)

    dump_file (f, s)


