#!@PYTHON@

"""
Print a nice footer.  add the top of the NEWS file (up to the ********)
"""


import os
import sys
import getopt
import __main__

lilypath =''
try:
    lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'
    sys.exit(1)

lilypath = lilypath + '/bin/'
sys.path.append(lilypath)
 
from lilypython import *

lilyversion= ''
fullname = "unknown"
news_file = ''

def set_vars():
    __main__.lilyversion =  version_tuple_to_str(lilydirs.version_tuple())
    os.environ["CONFIGSUFFIX"] = 'www';
    pw = pwd.getpwuid (os.getuid());

    __main__.fullname= pw[4];


backstr = '\n<hr>Please take me <a href=%s>back to the index</a>\n\
of LilyPond -- The GNU Project Music typesetter\n\
<hr><font size=-1>\n\
This page was built  from lilypond-%s by <p>\n\
<address><br>%s <a href=mailto:%s>&lt<!bla>%s</a>&gt</address>\n\
<p></font>' 

set_vars ()
banner_file = ''
news_file=''
news =''
(options, files) = getopt.getopt(sys.argv[1:], '', ['news=']) 

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--news':
	news_file = a

def footstr(index):
     return backstr % (index, lilyversion, fullname, mailaddress (), mailaddress)

index_file='../../out-www/index.html'
banner = footstr (index_file)
banner_id = '<! banner_id >'


if news_file:
    news = gulp_file (news_file)
    i = regex.search ('^\*\*', news)
    news = news[:i]
    
    
for f in files:
    s = gulp_file (f)
    if news_file:
	s = regsub.sub ('top_of_NEWS', '<XMP>\n'+ news  + '\n</XMP>\n', s)

    if regex.search (banner_id, s) == -1:
	s = regsub.sub ('</body>', '</BODY>', s)
	s = regsub.sub ('</BODY>', banner_id  + banner + '</BODY>', s)

    dump_file (f, s)
