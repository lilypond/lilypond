#!@PYTHON@

"""
Print a nice footer.  add the top of the ChangeLog file (up to the ********)
"""
import re
import sys
import os
import time
import string 
import getopt
import __main__

fullname = "unknown"
index_file=''
banner_file = ''
changelog_file=''
changes =''
package_version = ''

mail_address = '(address unknown)'
try:
	mail_address= os.environ['MAILADDRESS']
except KeyError:
	pass


webmaster= mail_address
try:
	webmaster= os.environ['WEBMASTER']
except KeyError:
	pass



footer_fn = ''
footer = r"""<hr>Please take me <a href=%s>back to the index</a>
of %s
<!-- package name %s>
 <!-- webmaster fields. %s %s>
"""

builtstr = r"""<hr><font size=-1>
This page was built from %s-%s by 
<address><br>%s &lt<a href=\"mailto:%s\">%s</a>&gt,  %s.</address><p></font>"""

package_name = ''

(options, files) = getopt.getopt(sys.argv[1:], 'c:hp:', [
	'name=', 'footer=', 'version=',
	'changelog=', 'help', 'news=', 'index=']) 

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

def help ():
	sys.stdout.write (r"""Usage: add-html-footer [OPTION]... HTML-FILE
Add a nice footer, add the top of the ChangLog file (up to the ********)
Options:
-h, --help                print this help
--version                 package version
--name                    package_name
--footer                  footer file
""")
	sys.exit (0)

for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--news' or o == '--changelog' or o == '-c':
		changelog_file = a
	elif o == '--index':
		index_file = a
	elif o == '--footer':
		footer_fn = a
	elif o == '--name':
		package_name = a
	elif o == '-h' or o == '--help':
		help ()
	elif o == '--version':
		package_version = a
	else:
		raise 'unknown opt ', o
def set_vars():
	os.environ["CONFIGSUFFIX"] = 'www';
	if os.name == 'nt':
		import ntpwd
		pw = ntpwd.getpwname(os.environ['USERNAME'])
	else:
		import pwd
		pw = pwd.getpwuid (os.getuid());

	f =pw[4]
	f = string.split (f, ',')[0]
	__main__.fullname=f 
set_vars ()


def footstr(index):
	ft = __main__.footer

	if footer_fn:
		try:
			ft = open (footer_fn).read ()
		except:
			raise 'oops: ' , footer_fn


	s = ft % (index, package_name, package_name, webmaster, webmaster)
	s = s + builtstr % (package_name, package_version, fullname,
			    mail_address, mail_address, 
			    time.strftime ('%c %Z', time.localtime (time.time ())))
	return s

banner = footstr (index_file)
banner_id = '<! banner_id >'




def do_file (s):

	if changelog_file:
		changes = gulp_file (changelog_file)
		# urg?
		#m = re.search ('^\\\\*\\\\*', changes)
		m = re.search (r'\*\*\*', changes)
		if m:
			changes = changes[:m.start (0)]
		s = re.sub ('top_of_ChangeLog', '<XMP>\n'+ changes  + '\n</XMP>\n', s)


	if re.search (banner_id, s) == None:
		s = banner_id + s
	else:
		return s

	s = re.sub ('(?i)<body>', '<BODY BGCOLOR=WHITE><FONT COLOR=BLACK>', s)
	# do title.
	#s = check_tag ('<body', '', s, 0)
	if re.search ('(?i)</body', s):
		s = re.sub ('(?i)</body>', banner + '</BODY>', s)
	elif re.search ('(?i)</html', s):		
		s = re.sub ('(?i)</html>', banner + '</HTML>', s)
	else:
		s = s + banner

	return s

for f in files:
	s = gulp_file (f)
	s = do_file (s)
	open (f, 'w').write (s)

if 0:
	title = '<HEAD><TITLE>' \
		+ package_name + ' -- ' + os.path.basename (os.path.splitext(f)[0]) \
		+ '</TITLE></HEAD>'
	s = check_tag ('<title>', title, s, 0)

	s = check_tag ('<html', '', s, 0)
	if regex.search ('<HTML', s) == -1:
		s = '<HTML>\n' + s
	s = check_tag ('</html>', '</HTML>', s, 1)

	dump_file (f, s)


