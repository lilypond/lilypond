#!/usr/bin/env python

"""
check_texi_refs.py
Interactive Texinfo cross-references checking and fixing tool


"""


import sys
import re
import os
import optparse

outdir = 'out-www'

log = sys.stderr
stdout = sys.stdout

warn_not_fixed = "*** Warning: this broken x-ref has not been fixed!\n"

opt_parser = optparse.OptionParser (usage='check_texi_refs.py [OPTION]... FILE',
                                    description='''Check and fix \
cross-references in a collection of Texinfo
documents heavily cross-referenced each other.
''')

opt_parser.add_option ('-a', '--auto-fix',
                       help="Automatically fix cross-references whenever \
it is possible",
                       action='store_true',
                       dest='auto_fix',
                       default=False)

opt_parser.add_option ('-b', '--batch',
                       help="Do not run interactively",
                       action='store_false',
                       dest='interactive',
                       default=True)

opt_parser.add_option ('-p', '--check-punctuation',
                       help="Check for punctuation after x-ref",
                       action='store_true',
                       dest='check_punctuation',
                       default=False)

(options, files) = opt_parser.parse_args ()

class InteractionError (Exception):
    pass


manuals = {}
exit_code = 0

def set_exit_code (n):
    global exit_code
    exit_code = max (exit_code, n)

if options.interactive:
    try:
        import readline
    except:
        pass

    def yes_prompt (question, default=False, retries=3):
        d = {True: 'y', False: 'n'}.get (default, False)
        while retries:
            a = raw_input ('%s [default: %s]' % (question, d) + '\n')
            if a.lower ().startswith ('y'):
                return True
            if a.lower ().startswith ('n'):
                return False
            if a == '' or retries < 0:
                return default
            stdout.write ("Please answer yes or no.\n")
            retries -= 1

    def search_prompt ():
        """Prompt user for a substring to look for in node names.

If user input is empty or matches no node name, return None,
otherwise return a list of (manual, node name, file) tuples.

"""
        substring = raw_input ("Enter a substring to search in node names \
(press Enter to skip this x-ref):\n")
        if not substring:
            return None
        substring = substring.lower ()
        matches = []
        for k in manuals:
            matches += [(k, node, manuals[k]['nodes'][node][0])
                        for node in manuals[k]['nodes']
                        if substring in node.lower ()]
        return matches

else:
    def yes_prompt (question, default=False, retries=3):
        return default

    def search_prompt ():
        return None

ref_re = re.compile (r'@(ref|ruser|rlearning|rprogram|rglos)\{([^,\\]*?)\}(.)',
                     re.DOTALL)
node_include_re = re.compile (r'(?m)^@(node|include)\s+(.+?)$')

whitespace_re = re.compile (r'\s+')
line_start_re = re.compile ('(?m)^')

manuals_to_refs = {'lilypond': 'ruser',
                   'lilypond-learning': 'rlearning',
                   'lilypond-program': 'rprogram',
                   # 'lilypond-snippets': 'rlsr',
                   'music-glossary': 'rglos'}

def which_line (index, newline_indices):
    """Calculate line number of a given string index

Return line number of string index index, where
newline_indices is an ordered iterable of all newline indices.
"""
    inf = 0
    sup = len (newline_indices) - 1
    n = len (newline_indices)
    while inf + 1 != sup:
        m = (inf + sup) / 2
        if index >= newline_indices [m]:
            inf = m
        else:
            sup = m
    return inf + 1

def read_file (f, d):
    """Look for all node names and cross-references in a Texinfo document

Return a dictionary with three keys:

  'manual' contains the cross-reference
macro name which matches the base name of f in global manuals_to_refs
dictionary,

  'nodes' is a dictionary of `node name':(file name, line number),

  'contents' is a dictionary of file:`full file contents'.

  'newline_indices' is a dictionary of
file:[list of beginning-of-line string indices]

Included files that can be read are processed too.

"""
    s = open (f).read ()
    # TODO (maybe as option)
    # s = trim_comments (s)
    base = os.path.basename (f)
    dir = os.path.dirname (f)
    if d == {}:
        d['manual'] = manuals_to_refs.get (os.path.splitext (base)[0], '')
        print "Processing manual %s(%s)" % (f, d['manual'])
        d['nodes'] = {}
        d['contents'] = {}
        d['newline_indices'] = {}

    d['newline_indices'][f] = [m.end () for m in line_start_re.finditer (s)]
    d['contents'][f] = s

    for m in node_include_re.finditer (s):
        if m.group (1) == 'node':
            line = which_line (m.start (), d['newline_indices'][f])
            d['nodes'][m.group (2)] = (f, line)

        elif m.group (1) == 'include':
            p = os.path.join (dir, m.group (2))
            if os.path.isfile (p):
                read_file (p, d)
            else:
                p = os.path.join (dir, outdir, m.group (2))
                if os.path.isfile (p):
                    read_file (p, d)
    return d

log.write ("Reading files...\n")

manuals = dict ([(d['manual'], d)
                 for d in [read_file (f, dict ()) for f in files]])

ref_fixes = set ()
fixes_count = 0

def add_fix (old_type, old_ref, new_type, new_ref):
    ref_fixes.add ((old_type, old_ref, new_type, new_ref))

def lookup_fix (r):
    found = []
    for (old_type, old_ref, new_type, new_ref) in ref_fixes:
        if r == old_ref:
            found.append ((new_type, new_ref))
    return found


def preserve_linebreak (text, linebroken):
    if linebroken:
        if ' ' in text:
            text = text.replace (' ', '\n', 1)
            n = ''
        else:
            n = '\n'
    else:
        n = ''
    return (text, n)

def choose_in_numbered_list (message, string_list, sep=' ', retries=3):
    S = set (string_list)
    S.discard ('')
    string_list = list (S)
    numbered_list = sep.join ([str (j + 1) + '. ' + string_list[j]
                               for j in range (len (string_list))]) + '\n'
    t = retries
    while t > 0:
        value = ''
        stdout.write (message +
                      "(press Enter to discard and start a new search)\n")
        input = raw_input (numbered_list)
        if not input:
            return ''
        try:
            value = string_list[int (input) - 1]
        except IndexError:
            stdout.write ("Error: index number out of range\n")
        except ValueError:
            matches = [input in v for v in string_list]
            n = matches.count (True)
            if n == 0:
                stdout.write ("Error: input matches no item in the list\n")
            elif n > 1:
                stdout.write ("Error: ambiguous input (matches several items \
in the list)\n")
            else:
                value = string_list[matches.index (True)]
        if value:
            return value
        t -= 1
    raise InteractionError ("%d retries limit exceeded" % retries)

def check_ref (manual, file, m):
    global fixes_count
    type = m.group (1)
    original_name = m.group (2)
    name = whitespace_re.sub (' ', original_name). strip ()
    newline_indices = manuals[manual]['newline_indices'][file]
    line = which_line (m.start (), newline_indices)
    linebroken = '\n' in m.group (2)
    next_char = m.group (3)

    # check for puncuation after x-ref
    if options.check_punctuation and not next_char in '.,;:!?':
        stdout.write ("Warning: %s: %d: `%s': x-ref not followed by punctuation\n"
                      % (file, line, name))

    # validate xref
    explicit_type = type
    new_name = name

    if type != 'ref' and type == manual:
        stdout.write ("\n%s: %d: `%s': external %s x-ref should be internal\n"
                      % (file, line, name, type))
        if options.auto_fix or yes_prompt ("Fix this?"):
            type = 'ref'

    if type == 'ref':
        explicit_type = manual

    if not name in manuals[explicit_type]['nodes']:
        fixed = False
        stdout.write ('\n')
        if type == 'ref':
            stdout.write ("%s: %d: `%s': wrong internal x-ref\n"
                          % (file, line, name))
        else:
            stdout.write ("%s: %d: `%s': wrong external `%s' x-ref\n"
                          % (file, line, name, type))
        # print context
        stdout.write ('--\n' + manuals[manual]['contents'][file]
                      [newline_indices[max (0, line - 2)]:
                       newline_indices[min (line + 3,
                                            len (newline_indices) - 1)]] +
                      '--\n')

        # try to find the reference in other manuals
        found = []
        for k in [k for k in manuals if k != explicit_type]:
            if name in manuals[k]['nodes']:
                if k == manual:
                    found = ['ref']
                    stdout.write ("  found as internal x-ref\n")
                    break
                else:
                    found.append (k)
                    stdout.write ("  found as `%s' x-ref\n" % k)

        if len (found) == 1 and (options.auto_fix
                                 or yes_prompt ("Fix this x-ref?")):
            add_fix (type, name, found[0], name)
            type = found[0]
            fixed = True

        elif len (found) > 1:
            if options.interactive or options.auto_fix:
                stdout.write ("* Several manuals contain this node name, \
cannot determine manual automatically.\n")
            if options.interactive:
                t = choose_in_numbered_list ("Choose manual for this x-ref by \
index number or beginning of name:\n", found)
                if t:
                    add_fix (type, name, t, name)
                    type = t
                    fixed = True

        if not fixed:
            # try to find a fix already made
            found = lookup_fix (name)

            if len (found) == 1:
                stdout.write ("Found one previous fix: %s `%s'\n" % found[0])
                if options.auto_fix or yes_prompt ("Apply this fix?"):
                    type, new_name = found[0]
                    fixed = True

            elif len (found) > 1:
                if options.interactive or options.auto_fix:
                    stdout.write ("* Several previous fixes match \
this node name, cannot fix automatically.\n")
                if options.interactive:
                    concatened = choose_in_numbered_list ("Choose new manual \
and x-ref by index number or beginning of name:\n", [''.join ([i[0], ' ', i[1]])
                                                     for i in found],
                                                    sep='\n')
                    if concatened:
                        type, new_name = concatenated.split (' ', 1)
                        fixed = True

        if not fixed:
            # all previous automatic fixes attempts failed,
            # ask user for substring to look in node names
            while True:
                node_list = search_prompt ()
                if node_list == None:
                    if options.interactive:
                        stdout.write (warn_not_fixed)
                    break
                elif not node_list:
                    stdout.write ("No matched node names.\n")
                else:
                    concatenated = choose_in_numbered_list ("Choose \
node name and manual for this x-ref by index number or beginning of name:\n", \
                            [' '.join ([i[0], i[1], '(in %s)' % i[2]]) for i in node_list],
                                                            sep='\n')
                    if concatenated:
                        t, z = concatenated.split (' ', 1)
                        new_name = z.split (' (in ', 1)[0]
                        add_fix (type, name, t, new_name)
                        type = t
                        fixed = True
                        break

    # compute returned string
    if new_name == name:
        return ('@%s{%s}' % (type, original_name)) + next_char
    else:
        fixes_count += 1
        (ref, n) = preserve_linebreak (new_name, linebroken)
        return ('@%s{%s}' % (type, ref)) + next_char + n


log.write ("Checking cross-references...\n")

try:
    for key in manuals:
        for file in manuals[key]['contents']:
            s = ref_re.sub (lambda m: check_ref (key, file, m),
                            manuals[key]['contents'][file])
            if s != manuals[key]['contents'][file]:
                open (file, 'w').write (s)

except KeyboardInterrupt:
    log.write ("Operation interrupted, exiting.\n")
    sys.exit (2)

except InteractionError, instance:
    log.write ("Operation refused by user: %s\nExiting.\n" % instance)
    sys.exit (3)

log.write ("Done, fixed %d x-refs.\n" % fixes_count)
