#!@PYTHON@

import __main__
import optparse
import gettext
import os
import re
import sys

verbose = 0
lang = 'C'
C = lang

def dir_lang (file, lang, lang_dir_index):
    path_components = file.split ('/')
    path_components[lang_dir_index] = lang
    return os.path.join (*path_components)

##     Translation of GIT Commit: <hash>
REVISION_RE = re.compile ('GIT [Cc]ommittish: ([a-f0-9]+)')
CVS_DIFF = 'git diff %(revision)s HEAD -- %(original)s | cat'

def check_file (original, translated):
    s = open (translated).read ()
    m = REVISION_RE.search (s)
    if not m:
        sys.stderr.write ('error: ' + translated + \
                          ": no 'GIT committish: <hash>' found.\nPlease check " + \
                          'the whole file against the original in English, then ' + \
                          'fill in HEAD committish in the header.\n')
        sys.exit (1)
    revision = m.group (1)

    c = CVS_DIFF % vars ()
    if verbose:
        sys.stderr.write ('running: ' + c)
    os.system (c)

def do_file (file_name, lang_codes):
    if verbose:
        sys.stderr.write ('%s...\n' % file_name)
    split_file_name = file_name.split ('/')
    d1, d2 = split_file_name[0:2]
    if d1 in lang_codes:
        check_lang = d1
        lang_dir_index = 0
    elif d2 in lang_codes:
        check_lang = d2
        lang_dir_index = 1
    else:
        check_lang = lang
    if check_lang == C:
        raise Exception ('cannot determine language for ' + file_name)
    
    original = dir_lang (file_name, '', lang_dir_index)
    translated = file_name
    check_file (original, translated)

def usage ():
    sys.stdout.write (r'''
Usage:
check-translation [--language=LANG] [--verbose] BUILDSCRIPT-DIR FILE...

This script is licensed under the GNU GPL.
''')

def do_options ():
    global lang, verbose

    p = optparse.OptionParser (usage="check-translation [--language=LANG] [--verbose] FILE...",
                               description="This script is licensed under the GNU GPL.")
    p.add_option ("--language",
                  action='store',
                  default='site',
                  dest="language")
    p.add_option ("--verbose",
                  action='store_true',
                  default=False,
                  dest="verbose",
                  help="the GIT directory to merge.")
    
    (options, files) = p.parse_args ()
    verbose = options.verbose
    lang = options.language
    
    return (files[0], files[1:])

def main ():
    import_path, files = do_options ()
    
    sys.path.append (import_path)
    import langdefs

    for i in files:
        do_file (i, langdefs.LANGDICT.keys())

if __name__ == '__main__':
    main ()
