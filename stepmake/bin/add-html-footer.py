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

gcos = "unknown"
index_url=''
top_url=''
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

default_footer = r"""<hr>Please take me <a href=@INDEX@>back to the index</a>
of @PACKAGE_NAME@
"""

built = r"""<hr><font size=-1>
This page was built from @PACKAGE_NAME@-@PACKAGE_VERSION@ by 
<address><br>@GCOS@ &lt<a href="mailto:%s">@MAIL_ADDRESS@</a>&gt,
@LOCALTIME@.</address><p></font>"""


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
	# urg, this is top!
	elif o == '--index':
		index_url = a
	elif o == '--name':
		package_name = a
	elif o == '--version':
		package_version = a
	else:
		raise 'unknown opt ', o

#burp?
def set_gcos ():
	global gcos
	os.environ["CONFIGSUFFIX"] = 'www';
	if os.name == 'nt':
		import ntpwd
		pw = ntpwd.getpwname(os.environ['USERNAME'])
	else:
		import pwd
		pw = pwd.getpwuid (os.getuid());

	f = pw[4]
	f = string.split (f, ',')[0]
	gcos = f 

def compose (default, file):
	s = default
	if file:
		s = gulp_file (file)
	return s

set_gcos ()
localtime = time.strftime ('%c %Z', time.localtime (time.time ()))

if os.path.basename (index_url) != "index.html":
	index_url = os.path.join (index_url , "index.html")
top_url = os.path.dirname (index_url) + "/"

header = compose (default_header, header_file)
footer = compose (default_footer, footer_file) + built
header_tag = '<! header_tag >'
footer_tag = '<! footer_tag >'

def do_file (f):
	s = gulp_file (f)

	if changelog_file:
		changes = gulp_file (changelog_file)
		# urg?
		#m = re.search ('^\\\\*\\\\*', changes)
		m = re.search (r'\*\*\*', changes)
		if m:
			changes = changes[:m.start (0)]
		s = re.sub ('top_of_ChangeLog', '<XMP>\n'+ changes  + '\n</XMP>\n', s)

	if re.search (header_tag, s) == None:
		body = '<BODY BGCOLOR=WHITE><FONT COLOR=BLACK>'
		s = re.sub ('(?i)<body>', body, s)
		if re.search ('(?i)<BODY', s):
			s = re.sub ('(?i)<body[^>]*>', body + header, s, 1)
		elif re.search ('(?i)<html', s):		
			s = re.sub ('(?i)<html>', '<HTML>' + header, s, 1)
		else:
			s = header + s

		s = header_tag + s

	if re.search (footer_tag, s) == None:
		s = s + footer_tag

		if re.search ('(?i)</body', s):
			s = re.sub ('(?i)</body>', footer + '</BODY>', s, 1)
		elif re.search ('(?i)</html', s):		
			s = re.sub ('(?i)</html>', footer + '</HTML>', s, 1)
		else:
			s = s + footer

	#URUGRGOUSNGUOUNRIU
	index = index_url
	top = top_url
	if os.path.basename (f) == "index.html":
		cwd = os.getcwd ()
		if os.path.basename (cwd) == "topdocs":
			index = "index.html"
			top = ""

		# don't cause ///////index.html entries in log files.
		#	index = "./index.html"
		#	top = "./"

	s = re.sub ('@INDEX@', index, s)
	s = re.sub ('@TOP@', top, s)
	s = re.sub ('@PACKAGE_NAME@', package_name, s)
	s = re.sub ('@PACKAGE_VERSION@', package_version, s)
	s = re.sub ('@WEBMASTER@', webmaster, s)
	s = re.sub ('@GCOS@', gcos, s)
	s = re.sub ('@LOCALTIME@', localtime, s)
	s = re.sub ('@MAIL_ADDRESS@', mail_address, s)

	open (f, 'w').write (s)


for f in files:
	do_file (f)

