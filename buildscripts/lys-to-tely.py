#!@PYTHON@


'''
TODO:

 * Add @nodes, plit at sections?
 * Less kludged first introduction file

'''


import sys
import os
import string 
import getopt

program_name = 'lys-to-tely'

def help ():
    sys.stdout.write (r"""Usage: lys-to-tely [OPTIONS]... LY-FILE...
Construct tely doc from LY-FILEs.

Options:
 -h, --help                print this help
 -n, --name=NAME         write tely doc to NAME
 -t, --title=TITLE         set tely doc title TITLE
 -i, --introduction=FILE   use FILE as intruduction at the top
 -f, --footer=FILE         use FILE as footer on the bottom of the page

""")
    sys.exit (0)

(options, files) = getopt.getopt(sys.argv[1:], 'hn:t:i:f:', [
    'help', 'name=', 'title=', 'introduction=', 'footer='])

name="ly-doc"
title="Ly Doc"
header = None
footer = None
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-h' or o == '--help':
        help ()
    elif o == '-n' or o == '--name':
        name = a
    elif o == '-t' or o == '--title':
        title = a
    elif o == '-i' or o == '--introduction':
        header = a
    elif o == '-f' or o == '--footer':
        footer = a
    else:
        raise 'unknown opt ', o

def strip_extension (f, ext):
    (p, e) = os.path.splitext (f)
    if e == ext:
        e = ''
    return p + e

if files:
    dir = os.path.dirname (name)
    if not dir:
        dir = "."
    name = strip_extension (os.path.basename (name), ".tely")

    s = '''\input texinfo
@setfilename %s.info
@settitle %s

@documentencoding utf-8
@iftex
@afourpaper
@end iftex

@finalout @c we do not want black boxes.
 
@c fool ls-latex
@ignore
@author Han-Wen Nienhuys and Jan Nieuwenhuizen
@title %s
@end ignore

@node Top, , , (dir)
''' % (name, title, title)

    if header:
        header_text = open (header).read ()
        s += header_text


    def name2line (n):
        # UGR
        s = r"""
@ifhtml
@html
<A NAME="%s"></A>
@end html
@end ifhtml
""" % n
        
        s += "\n\n@lilypondfile[printfilename,texidoc]{%s}" % n
        return s
    files.sort ()
    s = s + string.join (map (lambda x: name2line (x), files), "\n")
    s += '\n'
    if footer:
        footer_text = open (footer).read ()
        s += footer_text
        s += '\n'
    s = s + '@bye\n'
    f = "%s/%s.tely" % (dir, name)
    sys.stderr.write ("%s: writing %s..." % (program_name, f))
    h = open (f, "w")
    h.write (s)
    h.close ()
    sys.stderr.write ('\n')
else:
    # not Unix philosophy, but hey, at least we notice when
    # we don't distribute any .ly files.
    sys.stderr.write ("No files specified. Doing nothing")

