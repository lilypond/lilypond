#!@PYTHON@
# mutopia-index.py

name = 'mutopia-index'

import regex
import os
import sys
import stat
sys.path.append ('@abs-step-bindir@')



header_regex = regex.compile('\\header[ \t\n]*{\([^}]*\)}')
header_entry_regex = regex.compile('[\n\t ]*\([^\n\t ]+\)[\n\t ]*=[\n \t]*\([^;]+\)[\n \t]*;')

#
# FIXME breaks on multiple strings.
#
def read_mudela_header (fn):
	s = gulp_file(fn)
	s = regsub.gsub('%.*$', '', s)
	s = regsub.gsub('\n', ' ', s)	

	dict = {}
	if header_regex.search(s) <> -1:
		h = header_regex.group(1)
	else:
		return dict

	while regex.search('=', h) <> -1: 

		if header_entry_regex.search (h) == -1:

			raise 'format error'

		h = regsub.sub(header_entry_regex, '', h)
		left = header_entry_regex.group(1)
		right = header_entry_regex.group(2)

		right = regsub.gsub('\([^\\]\)\"', '\\1', right)
		right = regsub.gsub('^"', '', right)		
		left = regsub.gsub('\([^\\]\)\"', '', left)
		left = regsub.gsub('^"', '', left)

		dict[left] = right

	return dict
   



def help ():
    sys.stdout.write ("Usage: " + name + " [options] INFILE OUTFILE\n"
		 + "Generate index for mutopia\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -p, --package=DIR      specify package\n"
		 + "  --prefix=PRE           specify prefix\n"
		 + "  -s, --subdirs=DIR      add subdir\n"
		 + "  --suffix=SUF           specify suffix\n"
		      )
    sys.exit (0)

def gen_list(inputs, subdir, filename):
    (pre, subdirs, post)=subdir
    print "generating HTML list %s\n" % filename
    list = open(filename, 'w')
    list.write ('<html><TITLE>Rendered Examples</TITLE>\n')
    list.write ('<body bgcolor=white>')
    if len(subdirs):
	list.write  ('<h2>subdirectories</h2>')
	list.write  ('<ul>')	
        for ex in subdirs:
	    print 'subdir %s ' % ex
	    list.write ('<li><a href=%s/index.html>Subdirectory: %s</a></li>\n' % (pre + ex + post , ex))

	list.write ('</ul>')

    list.write('<h2>Contents of this directory</h2>\n');
    list.write (
    'These example files are taken from the LilyPond distribution. '
    'LilyPond currently only outputs TeX and MIDI.  The pictures and '
    'PostScript files were generated using TeX, Ghostscript and some '
    'graphics tools.  The papersize used for these examples is A4. '
    'As you know, <a href="http://www.gnu.org/philosophy/gif.html">no gifs due to patent problems</a>, '
    'but the png images should be viewable with any current browser '
    '(jpeg is inappropriate for music images).'
    '\n');


    for ex in inputs:
	ex_ext = '.ly'
	print '%s, ' % ex
	try:
	    header = read_mudela_header(ex + ex_ext + '.txt')
	except:
	    ex_ext = '.fly'
	    header = read_mudela_header(ex + ex_ext + '.txt')
	
	def read_dict(s, default, h =header):
		try:
		    ret = h[s]
		except KeyError:
		    ret = default
	        return ret
	head = read_dict('title', ex)
	composer = read_dict('composer', '')
	desc = read_dict('description', '')
	list.write('<hr>')
	list.write('<h1>example file: %s</h1>' % head);
	if composer <> '':
	    list.write('<h2>%s</h2>\n' % composer)
	if desc <> '':
	    list.write('%s<p>' % desc)
	list.write ('<ul>')
	def list_item(filename, desc, type, l = list):
	    if file_exist_b(filename):
		l.write ('<li><a href=%s>%s</a>' % (filename, desc))
		size=os.stat(filename)[stat.ST_SIZE]
		l.write (' (%s %dk)' % (type, (size + 512) / 1024))
		pictures = ['jpeg', 'png', 'xpm']
		# silly, no?
		if 0 and type in pictures:
		    l.write (' <a href="http://www.gnu.org/philosophy/gif.html">no gifs due to patent problems</a>')
		l.write ('\n')
	list_item(ex + ex_ext + '.txt', 'The input', 'ASCII')
	for pageno in range(1,100):
	    f  = ex + '-page%d.png' % pageno
	    if not file_exist_b (f):
		break
	    list_item(f, 'The output, page %d' % pageno, 'png')
	list_item(ex + '.ps.gz', 'The output', 'gzipped PostScript')
	list_item(ex + '.midi', 'The output', 'MIDI')
	list.write ("</ul>");

    list.write( "</BODY></HTML>");
    list.close()

import getopt

(options, files) = getopt.getopt(sys.argv[1:], 
  'hp:s:', ['help', 'subdirs=', 'suffix=', 'package=', 'prefix='])
subdir_pre=''
subdir_suf =''

subdirs =[]
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--subdirs' or o == '-s':
	subdirs.append (a)
    elif o == '--prefix':
	subdir_pre = a
    elif o == '-p' or o == '--package':
	topdir = a
    elif o == '--suffix':
	subdir_suf = a

    sys.path.append (topdir + '/stepmake/bin')
    from packagepython import *
    package = Package (topdir)
    packager = Packager ()

    from flower import *

	
# huh?
allfiles = multiple_find (['*.*ly.txt'], '.')

gen_list (files, (subdir_pre, subdirs, subdir_suf), 'index.html')

