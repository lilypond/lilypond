#!@PYTHON@


'''
TODO:

 * Add @nodes, plit at sections?
 * Less kludged first introduction file
 * include *.texi files for text at start of section?

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
 -o, --output=NAME         write tely doc to NAME
 -t, --title=TITLE         set tely doc title TITLE

""")
    sys.exit (0)

(options, files) = getopt.getopt(sys.argv[1:], 'hn:t:', [
    'help', 'name=', 'title='])

name="ly-doc"
title="Ly Doc"
for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-h' or o == '--help':
        help ()
    elif o == '-n' or o == '--name':
        name = a
    elif o == '-t' or o == '--title':
        title = a
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
    s = s + '\n@bye\n'
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

