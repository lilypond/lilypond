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

fullname = "unknown"
index_file=''
changelog_file=''
package_name = ''
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

header_file = ''
footer_file = ''
default_header = r"""
"""

default_footer = r"""<hr>Please take me <a href=%s>back to the index</a>
of %s
<!-- package name %s>
 <!-- webmaster fields. %s %s>
"""

builtstr = r"""<hr><font size=-1>
This page was built from %s-%s by 
<address><br>%s &lt<a href="mailto:%s">%s</a>&gt,  %s.</address><p></font>"""


def gulp_file (f):
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
Add header, footer and top of ChangLog file (up to the ********) to HTML-FILE

Options:
  --changelog=FILE          use FILE as ChangeLog [ChangeLog]
  --footer=FILE             use FILE as footer
  --header=FILE             use FILE as header
  -h, --help                print this help
  --index=URL               set homepage to URL
  --name=NAME               set package_name to NAME
  --version=VERSION         set package version to VERSION
""")
	sys.exit (0)

(options, files) = getopt.getopt(sys.argv[1:], 'h', [
	'changelog=', 'footer=', 'header=', 'help', 'index=',
	'name=', 'version=']) 

for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--changelog':
		changelog_file = a
	elif o == '--footer':
		footer_file = a
	elif o == '--header':
		header_file = a
	elif o == '-h' or o == '--help':
		help ()
	elif o == '--index':
		index_file = a
	elif o == '--name':
		package_name = a
	elif o == '--version':
		package_version = a
	else:
		raise 'unknown opt ', o

def set_vars ():
	global fullname
	os.environ["CONFIGSUFFIX"] = 'www';
	if os.name == 'nt':
		import ntpwd
		pw = ntpwd.getpwname(os.environ['USERNAME'])
	else:
		import pwd
		pw = pwd.getpwuid (os.getuid());

	f = pw[4]
	f = string.split (f, ',')[0]
	fullname = f 

#burp
def compose_header ():
	global default_header
	head = default_header
	if header_file:
		head = gulp_file (header_file)
	return head

def compose_footer (index):
	global default_footer
	foot = default_footer

	if footer_file:
		foot = gulp_file (footer_file)

	s = foot % (index, package_name, package_name, webmaster, webmaster)
	s = s + builtstr % (package_name, package_version, fullname,
			    mail_address, mail_address, 
			    time.strftime ('%c %Z', time.localtime (time.time ())))
	return s

set_vars ()
header = compose_header ()
footer = compose_footer (index_file)
header_tag = '<! header_tag >'
footer_tag = '<! footer_tag >'

def do_file (s):
	if changelog_file:
		changes = gulp_file (changelog_file)
		# urg?
		#m = re.search ('^\\\\*\\\\*', changes)
		m = re.search (r'\*\*\*', changes)
		if m:
			changes = changes[:m.start (0)]
		s = re.sub ('top_of_ChangeLog', '<XMP>\n'+ changes  + '\n</XMP>\n', s)

	if re.search (header_tag, s) == None:
		body='<BODY BGCOLOR=WHITE><FONT COLOR=BLACK>'
		s = re.sub ('(?i)<body>', body, s)
		if re.search ('(?i)<BODY', s):
			s = re.sub ('(?i)<body[^>]*>', body + header, s)
		elif re.search ('(?i)<html', s):		
			s = re.sub ('(?i)<html>', '<HTML>' + header, s)
		else:
			s = header + s

		s = header_tag + s

	if re.search (footer_tag, s) == None:
		s = s + footer_tag

		if re.search ('(?i)</body', s):
			s = re.sub ('(?i)</body>', footer + '</BODY>', s)
		elif re.search ('(?i)</html', s):		
			s = re.sub ('(?i)</html>', footer + '</HTML>', s)
		else:
			s = s + footer

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


