#!@PYTHON@
# 
# make-website.py --  implement The lilypond WWW site
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
# 

""" 
 stupid script to generate WWW site.  

 The WWW site is my test-suite for LilyPond, I usually don't
 distribute versions that fail to complete this script """

import sys
import os

lilypath =''
try:
    lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except IndexError:
    lilypath = os.environ['HOME'] + 'musix/current'
lilypath = lilypath + '/bin/'
sys.path.append(lilypath)
 
from lilypython import *
import __main__
import glob

depth = ''
makewebsite_id = "<!make_website!>";
id_str = "make-website 0.7";
tar = "tar";
make = "make";
mailaddress = "unknown"
fullname = "unknown"
footstr = ""
lilyversion= ''

def set_vars():
    __main__.lilyversion =  version_tuple_to_str(lilydirs.version_tuple())
    os.environ["TEXINPUTS"] = os.environ["TEXINPUTS"] + ":%s/input/:" % depth;
    os.environ["LILYINCLUDE"] = "%s/input/" % depth;
    os.environ["LILYTOP"] = depth;
    __main__.mailaddress= os.environ['MAILADDRESS']
    pw = pwd.getpwuid (os.getuid());

    __main__.fullname= pw[4];

backstr = '\n<hr>Please take me <a href=%s>back to the index</a>\n\
of LilyPond -- The GNU Project Music typesetter\n\
<hr><font size=-1>\n\
This page was built using <code>%s</code> from lilypond-%s by <p>\n\
<address><br>%s <a href=mailto:%s&>>&lt<!bla>%s</a>&gt</address>\n\
<p></font>' 

    
def footstr(index):
     return backstr % (index, id_str, lilyversion, fullname, mailaddress, mailaddress)

    

# do something, check return status
def my_system(cmds):
    for cmd in cmds:
	ignoreret=0;
	if cmd[0] == '-':
	    ignoreret = 1
	    cmd = cmd[1:]
	
	ret = os.system (cmd)
	if ret:
	    if ignoreret: 
		sys.stderr.write( "ignoring failed command \`%s\' (status %d)\n" % (cmd, ret))
	    else:
		sys.stderr.write( 'make-website: failed on command %s (status %d)\n' % (cmd, ret))
		sys.exit (2)

base="lilypond/";

examples=["twinkle-pop", 
	  "wtk1-fugue2",
	  "standchen-16", 
	  "standchen-20", 
	  "standje",
	  "wtk1-prelude1",
	  "toccata-fuga-E", 
	  "scsii-menuetto",
	  "cadenza", 
	  "gallina",
	  "twinkle", 
	  "collisions",
	  "font16",
	  "font20",
	  #"scales", 
	  "rhythm", 
	  "multi"]

def gen_html():
    print 'generating HTML'
    my_system (["make -kC .. html"]);
    

def gen_examples():
    print 'generating examples:\n'
    list = map(lambda x: 'out/%s.ps.gz out/%s.gif out/%s.ly.txt' % (x,x,x), examples)
    my_system (['make -C .. ' + join(' ', list)])

texstuff = ["mudela-man", "mudela-course"]

def gen_manuals():
    print 'generating TeX doco manuals'
    list = open('tex_manuals.html', 'w')
    list.write( "<HTML><TITLE>PostScript Manuals</TITLE>\n" 
     "<BODY><h1>LilyPond manuals (in PostScript)</h1>"
     "<ul>\n")
    todo='' 
    for stuff in texstuff:
	todo = todo + ' out/' + stuff + '.ps.gz'
	list.write("<li><a href=%s.ps.gz>%s.ps.gz</a>" % (stuff, stuff))
    list.write('</ul></BODY></HTML>')
    list.close ()

    my_system (['make -C .. ' + todo])

def file_exist_b(name):
    try: 
	f = open(name)
    except IOError:
	return 0
    f.close ()
    return 1

def gen_list():
    print "generating HTML list\n";
    list = open('example_output.html', 'w')
    list.write ('<html><TITLE>Rendered Examples</TITLE>\n'
     '<body>These example files are taken from the LilyPond distribution.\n'
     'LilyPond currently only outputs TeX and MIDI. The pictures and\n'
     'PostScript files were generated using TeX, Ghostscript and some\n'
     'graphics tools.  The papersize used for these examples is A4.  The GIF\n'
     'files have been scaled to eliminate aliasing.\n');

    for ex in examples:
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

def copy_files():
    print "copying files\n"
    
#    my_system ("ln -s depth/out ./docxx" )
    my_system([ "cp %s/TODO ./TODO.txt" % depth,
    "cp %s/ANNOUNCE-0.1 ./ANNOUNCE.txt" % depth,
    "cp %s/NEWS ./NEWS.txt" % depth,
    "cp %s/DEDICATION ./DEDICATION.txt" % depth]);
    my_system([ "make -C .. gifs"]);
    
def docxx_update():
    print 'docxx.. \n'
    banner= open('/tmp/lilybanner.html', 'w');
    banner.write (footstr('../index.html'))
    banner.close ()
    my_system(['BANNEROPT=\"-B /tmp/lilybanner.html\" %s/bin/out/make-docxx' % depth]);
#    os.unlink( "/tmp/lilybanner.html");

def get_glob(exts):
    result = []
    for a in exts:
	result = result + glob.glob1('./', '*.' + a)

    return result

def join(inter, list):
    return reduce (lambda x,y, i=inter: x + i + y, list)

def do_tar():
     print "tarring.\n";
     list = get_glob( ['html', 'gif', 'ps.gz' , 'txt', 'midi']) 
     files = join (' ', list)
     print files
     my_system( ['-tar zvhcf website.tar.gz ' + files + ' docxx/*'])


def identify():
    print 'This is %s\n' % id_str
    
def clean_tmp():
    my_system(['rm -f /tmp/gs*'])
    
def get_top_of_NEWS():
    i = open('NEWS.txt')
    lines = i.readlines()
    i.close ()
    s = ''
    for l in lines:
	if regex.search('^\*\*\*\*\*\*', l) <> -1:
	    return s;
	s = s + l
    return s

def edit_index():
    s = gulp_file('index.html')
    s = regsub.sub ('top_of_NEWS', '<XMP>\n' + get_top_of_NEWS () + '\n</XMP>', s)
    dump_file ('index.html', s)

def edit_html():
    files = get_glob(['html'])
    for f in files:

	s = gulp_file(f)
	if regex.search(makewebsite_id, s) <> -1:
	    continue;

	s = regsub.sub ('</BODY>', footstr('index.html') + makewebsite_id + '</BODY>', s)
	s = regsub.sub('<TITLE>\(.*\)</TITLE>$', 
		       '<TITLE>LilyPond WWW: \\1</TITLE>', s)
	dump_file (f,s)

def main():
    identify();

    os.chdir (lilydirs.topdir + 'Documentation/out')
    __main__.depth = "../../";

    set_vars();
    gen_html();
    copy_files();
    gen_examples();
    gen_list();
    gen_manuals();
    #set_images();
    edit_html();
    edit_index();
    docxx_update()
    do_tar()
    clean_tmp();


main()


