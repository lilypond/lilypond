#!@PYTHON@

"""
Print a nice footer.  add the top of the NEWS file (up to the ********)
"""

program_name = 'add-html-footer'
version = '0.1'

import sys
import os
from string import *
import getopt
import __main__

fullname = "unknown"
news_file = ''

index_file=''
banner_file = ''
news_file=''
news =''
(options, files) = getopt.getopt(sys.argv[1:], 'hp:', ['help', 'news=', 'index=', 'package=']) 

def help ():
    sys.stdout.write ("Usage: add-html-footer [OPTION]... HTML-FILE\n"
		 "Add a nice footer, add the top of the NEWS file (up to the ********)\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -p, --package=DIR      specify package\n"
		      )
    sys.exit (0)

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--news':
	news_file = a
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

backstr = '\n<hr>Please take me <a href=%s>back to the index</a>\n\
of ' + package.Name + '\n'
builtstr = '\n<hr><font size=-1>\n\
This page was built  from ' + package.name + '-%s by <p>\n\
<address><br>%s &lt<a href=mailto:%s>%s</a>&gt</address>\n\
<p></font>' 

def footstr(index):
    s = backstr % index
    s = s + builtstr % (version_tuple_to_str (package.version), fullname,
		         packager.mail, packager.mail)
    return s

banner = footstr (index_file)
banner_id = '<! banner_id >'


if news_file:
    news = gulp_file (news_file)
    i = regex.search ('^\*\*', news)
    news = news[:i]
    
def check_tag (tag, sub, s, bottom):
    tag = lower (tag)
    TAG = upper (tag)
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

    if news_file:
	s = regsub.sub ('top_of_NEWS', '<XMP>\n'+ news  + '\n</XMP>\n', s)

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

    s = check_tag ('<html>', '<HTML>', s, 0)
    s = check_tag ('</html>', '</HTML>', s, 1)

    #urg
    if regex.search ('@COUNTER_REF@', s) != -1:
	counter = ''
	try:
	    counter = os.environ[package.NAME + '_COUNTERPATH']
	    counter = '<hr><img src="' + counter + '">\n'
	except:
	    pass
	s = regsub.gsub ('@COUNTER_REF@', counter, s)

    dump_file (f, s)


