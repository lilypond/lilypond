import time
import re
import sys
import getopt
import os

def usage ():
	sys.stderr.write ('''
texi2omf [options] texifile

Options:

--format=FORM         format is FORM. Supported: HTML, PS, PDF
--location=FILE       path to file on disk.
--version=VERSION

Use the following commands (enclose in @ignore)

@omfsubject . .
@omfdescription . .
@omftype . . 

etc.


''')
	
(options, files) = getopt.getopt (sys.argv[1:], '',
				  ['format=', 'location=', 'version='])

license = 'FDL'
location = ''
version = ''
email = os.getenv ('MAILADDRESS')
name = os.getenv ('USERNAME')

for (o,a) in options:
	if o == '--format':
		format = a
	elif o == '--location':
		location = 'file:%s' % a
	elif o == '--version':
		version = a
	else:
		assert 0

		
if not files:
	usage()
	sys.exit (2)


formats = {
	'html' : 'text/html',
	'pdf' : 'application/pdf',
	'ps' : 'application/postscript',
	}

if not formats.has_key (format):
	sys.stderr.write ("Format `%s' unknown\n" % format)
	sys.exit (1)


infile =files[0]

today = time.localtime()

texi = open (infile).read()

if not location:
	location = 'file:%s' % re.sub (r'\.*', '.html', infile)
	

omf_vars = {
	'date': '%d-%d-%d' %  today[:3],
	'mimeformat': formats[format],
	'maintainer':  "%s (%s)" % (name, email),
	'version' : version,
	'location' : location, 
	}

for a in ['subject','creator', 'title', 'subtitle', 'version', 'category', 'type',
	  'description',  'license']:
	
	m = re.search ('@omf%s (.*)\n'% a, texi)
	if m:
		omf_vars[a] = m.group (1)
	elif not omf_vars.has_key (a):
		omf_vars[a] = ''
		
if not omf_vars['title']:
	title = ''
	m = re.search ('@title (.*)\n', texi)
	if m:
		title = m.group (1)

	subtitle = ''
	m = re.search ('@subtitle (.*)\n', texi)
	if m:
		subtitle = m.group (1)

	if subtitle:
		title  = '%s -- %s' % (title, subtitle)

	omf_vars['title'] = title
	
if not omf_vars['creator']:
	m = re.search ('@author (.*)\n', texi)
	if m:
		omf_vars['creator'] = m.group (1)



print r'''
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE omf PUBLIC "-//OMF//DTD Scrollkeeper OMF Variant V1.0//EN" "http://scrollkeeper.sourceforge.net/dtds/scrollkeeper-omf-1.0/scrollkeeper-omf.dtd">
<omf>
  <resource>
    <creator>
      %(creator)s
    </creator>
    <maintainer>
      %(maintainer)s
    </maintainer>
    <title>
      %(title)s
    </title>
    <date>
      %(date)s
    </date>
    <version identifier="%(version)s" />
    <subject category="%(category)s"/>
    <description>
    %(description)s
    </description>
    <type>
    %(type)s
    </type>
    <format mime="%(mimeformat)s">
    <identifier url="%(location)s"/>
    <language code="C"/>
    <rights type="%(license)s" />
  </resource>
</omf>
''' % omf_vars


