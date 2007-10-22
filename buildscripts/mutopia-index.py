#!@PYTHON@
# mutopia-index.py

import fnmatch
import getopt
import os
import os
import re
import stat
import sys

def find (pat, dir):
    f = os.popen ('find %s -name "%s"'% (dir, pat))
    lst = []
    for a in f.readlines():
        a = a[:-1]
        lst.append (a)
    return lst


junk_prefix = 'out-www/'

headertext= r"""

<h1>LilyPond samples</h1>


<p>You're looking at a page with some LilyPond samples.  These files
are also included in the distribution. The output is completely
generated from the source file, without any further touch up.

<p>

The pictures are 90 dpi anti-aliased snapshots of the printed output.
For a good impression of the quality print out the PDF file.
"""

headertext_nopics= r"""
<p>No examples were found in this directory. 
"""

#
# FIXME breaks on multiple strings.
#
def read_lilypond_header (fn):
    s = open (fn).read ()
    s = re.sub ('%.*$', '', s)
    s = re.sub ('\n', ' ', s)                

    dict = {}
    m = re.search (r"""\\header\s*{([^}]*)}""", s)

    if m:
            s = m.group (1)
    else:
            return dict

    while s:
        m = re.search (r'''\s*(\S+)\s*=\s*"([^"]+)"''', s)
        if m == None:
            s = ''
        else:
            s = s[m.end (0):]
            left  = m.group         (1)
            right = m.group (2)

            left = re.sub ('"', '', left)
            right = re.sub ('"', '', right)
            dict[left] = right

    return dict

def help ():
    sys.stdout.write (r'''Usage: mutopia-index [OPTIONS] INFILE OUTFILE
Generate index for mutopia.

Options:
 -h, --help                 print this help
 -o, --output=FILE          write output to file
 -s, --subdirs=DIR             add subdir
   --suffix=SUF             specify suffix
   
''')
    sys.exit (0)

# ugh.
def gen_list (inputs, file_name):
    sys.stderr.write ("generating HTML list %s" % file_name)
    sys.stderr.write ('\n')
    if file_name:
        list = open (file_name, 'w')
    else:
        list = sys.stdout
    list.write ('''<html><head><title>Rendered Examples</title>
<style type="text/css">
hr { border:0; height:1; color: #000000; background-color: #000000; }\n
</style>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
</head>''')

    list.write ('<body bgcolor=white>\n')
    
    if inputs:
        list.write (headertext)
    else:
        list.write (headertext_nopics)

    for ex in inputs:
        print ex
        
        (base, ext) = os.path.splitext (ex)
        (base, ext2) = os.path.splitext (base)                
        ext = ext2 + ext
        
        header = read_lilypond_header (ex)
        def read_dict (s, default, h = header):
                try:
                    ret = h[s]
                except KeyError:
                    ret = default
                return ret
        head = read_dict ('title', os.path.basename (base))
        composer = read_dict ('composer', '')
        desc = read_dict ('description', '')
        list.write ('<hr>\n')
        list.write ('<h1>%s</h1>\n' % head);
        if composer:
            list.write ('<h2>%s</h2>\n' % composer)
        if desc:
            list.write ('%s<p>' % desc)
        list.write ('<ul>\n')

        def list_item (file_name, desc, type, lst = list):
            if os.path.isfile (file_name):
                lst.write ('<li><a href="%s">%s</a>'
                     % (re.sub (junk_prefix, '', file_name), desc))

                # FIXME: include warning if it uses \include
                # files.
                
                size = os.stat (file_name)[stat.ST_SIZE]
                kB = (size + 512) / 1024
                if kB:
                    lst.write (' (%s %d kB)' % (type, kB))
                else:
                    lst.write (' (%s %d characters)'
                         % (type, size))
                pictures = ['jpeg', 'png', 'xpm']
                lst.write ('\n')
            else:
                print "cannot find" , `file_name`

        list_item (base + ext, 'The input', 'ASCII')

        pages_found = 0
        for page in range (1, 100):
            f = base + '-page%d.png' % page
            
            if not os.path.isfile (f):
                break
            pages_found += 1
            list_item (f, 'See a picture of page %d' % page, 'png')

        if pages_found == 0 and os.path.exists (base + '.png'):
            list_item (base + ".png",
                 'See a picture', 'png')

            
        list_item (base + '.pdf', 'Print', 'PDF')
        list_item (base + '.midi', 'Listen', 'MIDI')
        list.write ('</ul>\n');

    list.write ('</body></html>\n');
    list.close ()

(options, files) = getopt.getopt (sys.argv[1:], 
 'ho:', ['help', 'output='])
outfile = 'examples.html'

subdirs = []
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--help' or o == '-h':
        help ()
    elif o == '--output' or o == '-o':
        outfile = a

dirs  = []
for f in files:
    dirs = dirs + find ('out-www', f)

if not dirs:
    dirs = ['.']

allfiles = []

for d in dirs:
    allfiles = allfiles + find ('*.ly', d)

allfiles = filter (lambda x: not x.endswith ('snippet-map.ly') and not re.search ('lily-[0-9a-f]+', x), allfiles)
allfiles = filter (lambda x: 'musicxml' not in x, allfiles)


gen_list (allfiles, outfile)

