
import sys
import os

lilypath =''
try:
    lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'
    sys.exit(1)

lilypath = lilypath + '/bin/'
sys.path.append(lilypath)
 
from lilypython import *



def gen_list(inputs, subdir, filename):
    (pre, subdirs, post)=subdir
    print "generating HTML list %s\n" % filename
    list = open(filename, 'w')
    list.write ('<html><TITLE>Rendered Examples</TITLE>\n')
    list.write ('<body>')
    if len(subdirs):
	list.write  ('<h2>subdirectories</h2>')
	list.write  ('<ul>')	
        for ex in subdirs:
	    print 'subdir %s ' % ex
	    list.write ('<li><a href=%s>Subdirectory: %s</a></li>\n' % (pre + ex + post , ex))

	list.write ('</ul>')

    list.write('<h2>Contents of this directory</h2>\n');
    list.write ('These example files are taken from the LilyPond distribution.\n'
     'LilyPond currently only outputs TeX and MIDI. The pictures and\n'
     'PostScript files were generated using TeX, Ghostscript and some\n'
     'graphics tools.  The papersize used for these examples is A4.  The GIF\n'
     'files have been scaled to eliminate aliasing.\n');


    for ex in inputs:
	print '%s, ' % ex
	header  = read_mudela_header(ex + '.ly.txt')
	
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
	def list_item(filename, desc, l = list):
	    if file_exist_b(filename):
		l.write ('<li><a href=%s>%s</a>\n' % (filename, desc))
	    
	list_item(ex + '.ly.txt', 'The input')
	for pageno in range(1,10):
	    f  = ex + '-page%d.gif' % pageno
	    if not file_exist_b (f):
		break
	    list_item(f, 'The output (picture, page %d)' % pageno)
	list_item(ex + '.ps.gz', 'The output (gzipped PostScript)')
	list_item(ex + '.midi', 'The output (MIDI)')
	list.write ("</ul>");

    list.write( "</BODY></HTML>");
    list.close()

allfiles = multiple_find (['*.ly.txt'], '.')

import getopt

(cl_options, files) = getopt.getopt(sys.argv[1:], 
				    'hs:', ['help', 'subdirs=', 'suffix=', 'prefix='])
subdir_pre=''
subdir_suf =''

subdirs =[]
for opt in cl_options:
    o = opt[0]
    a = opt[1]
    if o == '--subdirs' or o == '-s':
	subdirs.append (a)
    elif o == '--prefix':
	subdir_pre = a
    elif o == '--suffix':
	subdir_suf = a
	
gen_list (files, (subdir_pre, subdirs, subdir_suf), 'index.html')
