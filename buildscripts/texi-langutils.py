#!@PYTHON@
# texi-langutils.py

# WARNING: this script can't find files included in a different directory

import sys
import re
import getopt
import os

import langdefs

def read_pipe (command):
    print command
    pipe = os.popen (command)
    output = pipe.read ()
    if pipe.close ():
        print "pipe failed: %(command)s" % locals ()
    return output


optlist, texi_files = getopt.getopt(sys.argv[1:],'no:d:b:i:l:',['skeleton', 'gettext'])
process_includes = not ('-n', '') in optlist # -n   don't process @include's in texinfo files

make_gettext = ('--gettext', '') in optlist   # --gettext    generate a node list from a Texinfo source
make_skeleton = ('--skeleton', '') in optlist # --skeleton   extract the node tree from a Texinfo source

output_file = 'doc.pot'

# @untranslated should be defined as a macro in Texinfo source
node_blurb = '''@untranslated
'''
doclang = ''
head_committish = read_pipe ('git-rev-parse HEAD')
intro_blurb = '''@c -*- coding: utf-8; mode: texinfo%(doclang)s -*-
@c This file is part of %(topfile)s
@ignore
    Translation of GIT committish: %(head_committish)s
    When revising a translation, copy the HEAD committish of the
    version that you are working on.  See TRANSLATION for details.
@end ignore
'''

end_blurb = """
@c -- SKELETON FILE --
"""

for x in optlist:
    if x[0] == '-o': # -o NAME   set PO output file name to NAME
        output_file = x[1]
    elif x[0] == '-d': # -d DIR    set working directory to DIR
        os.chdir (x[1])
    elif x[0] == '-b': # -b BLURB  set blurb written at each node to BLURB
        node_blurb = x[1]
    elif x[0] == '-i': # -i BLURB  set blurb written at beginning of each file to BLURB
        intro_blurb = x[1]
    elif x[0] == '-l': # -l ISOLANG  set documentlanguage to ISOLANG
        doclang = '; documentlanguage: ' + x[1]

texinfo_with_menus_re = re.compile (r"^(\*) +([^:\n]+)::.*?$|^@(include|menu|end menu|node|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading) *(.*?)$|@(rglos){(.+?)}", re.M)

texinfo_re = re.compile (r"^@(include|node|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading) *(.+?)$|@(rglos){(.+?)}", re.M)

ly_string_re = re.compile (r'^([a-zA-Z]+)[\t ]*=|%+[\t ]*(.*)$')
verbatim_ly_re = re.compile (r'@lilypond\[.*?verbatim')

def process_texi (texifilename, i_blurb, n_blurb, write_skeleton, topfile, output_file=None, scan_ly=False):
    try:
        f = open (texifilename, 'r')
        texifile = f.read ()
        f.close ()
        printedfilename = texifilename.replace ('../','')
        includes = []

        # process ly var names and comments
        if output_file and scan_ly:
            lines = texifile.splitlines ()
            i = 0
            in_verb_ly_block = False
            for i in range (len (lines)):
                if verbatim_ly_re.match (lines[i]):
                    in_verb_ly_block = True
                elif lines[i].startswith ('@end lilypond'):
                    in_verb_ly_block = False
                elif in_verb_ly_block:
                    for (var, comment) in ly_string_re.findall (lines[i]):
                        if var:
                            output_file.write ('# ' + printedfilename + ':' + \
                                               str (i + 1) + ' (variable)\n_(r"' + var + '")\n')
                        elif comment:
                            output_file.write ('# ' + printedfilename + ':' + \
                                               str (i + 1) + ' (comment)\n_(r"' + \
                                               comment.replace ('"', '\\"') + '")\n')

        # process Texinfo node names and section titles
        if write_skeleton:
            g = open (os.path.basename (texifilename), 'w')
            subst = globals ()
            subst.update (locals ())
            g.write (i_blurb % subst)
            tutu = texinfo_with_menus_re.findall (texifile)
            node_trigger = False
            for item in tutu:
                if item[0] == '*':
                    g.write ('* ' + item[1] + '::\n')
                elif output_file and item[4] == 'rglos':
                    output_file.write ('_(r"' + item[5] + '") # @rglos in ' + printedfilename + '\n')
                elif item[2] == 'menu':
                    g.write ('@menu\n')
                elif item[2] == 'end menu':
                    g.write ('@end menu\n\n')
                else:
                    g.write ('@' + item[2] + ' ' + item[3] + '\n')
                    if node_trigger:
                        g.write (n_blurb)
                        node_trigger = False
                    elif item[2] == 'include':
                        includes.append (item[3])
                    else:
                        if output_file:
                            output_file.write ('# @' + item[2] + ' in ' + \
                                printedfilename + '\n_(r"' + item[3].strip () + '")\n')
                        if item[2] == 'node':
                            node_trigger = True
            g.write (end_blurb)
            g.close ()

        elif output_file:
            toto = texinfo_re.findall (texifile)
            for item in toto:
                if item[0] == 'include':
                    includes.append(item[1])
                elif item[2] == 'rglos':
                    output_file.write ('# @rglos in ' + printedfilename + '\n_(r"' + item[3] + '")\n')
                else:
                    output_file.write ('# @' + item[0] + ' in ' + printedfilename + '\n_(r"' + item[1].strip () + '")\n')

        if process_includes:
            dir = os.path.dirname (texifilename)
            for item in includes:
                process_texi (os.path.join (dir, item.strip ()), i_blurb, n_blurb, write_skeleton, topfile, output_file, scan_ly)
    except IOError, (errno, strerror):
        sys.stderr.write ("I/O error(%s): %s: %s\n" % (errno, texifilename, strerror))


if intro_blurb != '':
    intro_blurb += '\n\n'
if node_blurb != '':
    node_blurb = '\n' + node_blurb + '\n\n'
if make_gettext:
    node_list_filename = 'node_list'
    node_list = open (node_list_filename, 'w')
    node_list.write ('# -*- coding: utf-8 -*-\n')
    for texi_file in texi_files:
        # Urgly: scan ly comments and variable names only in English doco
        is_english_doc = 'Documentation/user' in texi_file
        process_texi (texi_file, intro_blurb, node_blurb, make_skeleton,
                      os.path.basename (texi_file), node_list,
                      scan_ly=is_english_doc)
    for word in ('Up:', 'Next:', 'Previous:', 'Appendix ', 'Footnotes', 'Table of Contents'):
        node_list.write ('_(r"' + word + '")\n')
    node_list.close ()
    os.system ('xgettext -c -L Python --no-location -o ' + output_file + ' ' + node_list_filename)
else:
    for texi_file in texi_files:
        process_texi (texi_file, intro_blurb, node_blurb, make_skeleton,
                      os.path.basename (texi_file))
