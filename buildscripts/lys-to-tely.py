#!@PYTHON@


'''
TODO:

 * Add @nodes, split at sections?

'''


import sys
import os
import getopt

program_name = 'lys-to-tely'

include_snippets = '@lysnippets'
fragment_options = 'printfilename,texidoc'
help_text = r"""Usage: %(program_name)s [OPTIONS]... LY-FILE...
Construct tely doc from LY-FILEs.

Options:
 -h, --help                     print this help
 -f, --fragment-options=OPTIONS use OPTIONS as lilypond-book fragment
   options
 -o, --output=NAME              write tely doc to NAME
 -t, --title=TITLE              set tely doc title TITLE
     --template=TEMPLATE        use TEMPLATE as Texinfo template file,
   instead of standard template; TEMPLATE should contain a command
   '%(include_snippets)s' to tell where to insert LY-FILEs.  When this
   option is used, NAME and TITLE are ignored.
"""

def help (text):
    sys.stdout.write ( text)
    sys.exit (0)

(options, files) = getopt.getopt (sys.argv[1:], 'f:hn:t:',
                     ['fragment-options=', 'help', 'name=', 'title=', 'template='])

name = "ly-doc"
title = "Ly Doc"
template = '''\input texinfo
@setfilename %%(name)s.info
@settitle %%(title)s

@documentencoding utf-8
@iftex
@afourpaper
@end iftex

@finalout @c we do not want black boxes.

@c fool ls-latex
@ignore
@author Han-Wen Nienhuys and Jan Nieuwenhuizen
@title %%(title)s
@end ignore

@node Top, , , (dir)
@top %%(title)s

%s

@bye
''' % include_snippets

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-h' or o == '--help':
        # We can't use vars () inside a function, as that only contains all 
        # local variables and none of the global variables! Thus we have to 
        # generate the help text here and pass it to the function...
        help (help_text % vars ())
    elif o == '-n' or o == '--name':
        name = a
    elif o == '-t' or o == '--title':
        title = a
    elif o == '-f' or o == '--fragment-options':
        fragment_options = a
    elif o == '--template':
        template = open (a, 'r').read ()
    else:
        raise Exception ('unknown option: ' + o)

def name2line (n):
    s = r"""
@ifhtml
@html
<a name="%s"></a>
@end html
@end ifhtml

@lilypondfile[%s]{%s}
""" % (os.path.basename (n), fragment_options, n)
    return s

if files:
    dir = os.path.dirname (name) or "."
# don't strip .tely extension, input/lsr uses .itely
    name = os.path.basename (name)
    template = template % vars ()

    files.sort ()
    s = "\n".join (map (name2line, files))
    s = template.replace (include_snippets, s, 1)
    f = "%s/%s" % (dir, name)
    sys.stderr.write ("%s: writing %s..." % (program_name, f))
    h = open (f, "w")
    h.write (s)
    h.close ()
    sys.stderr.write ('\n')
else:
    # not Unix philosophy, but hey, at least we notice when
    # we don't distribute any .ly files.
    sys.stderr.write ("No files specified. Doing nothing")
