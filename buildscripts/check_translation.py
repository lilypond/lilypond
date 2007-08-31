#!@PYTHON@

import __main__
import optparse
import gettext
import os
import re
import sys

verbose = 0
lang = 'site'
C = lang

def dir_lang (file, lang):
    path_components = [lang] + file.split ('/')[1:]
    return os.path.join (*path_components)

##     Translation of GIT Commit: <hash>
REVISION_RE = re.compile ('.*GIT [Cc]ommittish: ([a-f0-9]+)', re.DOTALL)
CVS_DIFF = 'git diff %(revision)s HEAD -- %(original)s | cat'

def check_file (original, translated):
    s = open (translated).read ()
    m = REVISION_RE.match (s)
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

def do_file (file_name, langdefs):
    if verbose:
        sys.stderr.write ('%s...\n' % file_name)
    file_lang = file_name.split ('/')[0]
    if file_lang in langdefs.LANGDICT.keys():
        check_lang = file_lang
    else:
        check_lang = lang
    if check_lang == C:
        raise 'cannot determine language for: ' + file_name
    
    original = dir_lang (file_name, '')
    translated = dir_lang (file_name, check_lang)
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
        do_file (i, langdefs)

if __name__ == '__main__':
    main ()
