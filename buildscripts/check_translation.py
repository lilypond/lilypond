#!/usr/bin/env python

import __main__
import optparse
import os
import sys

import langdefs
import buildlib

verbose = 0
use_colors = False
lang = 'C'
C = lang

def dir_lang (file, lang, lang_dir_index):
    path_components = file.split ('/')
    path_components[lang_dir_index] = lang
    return os.path.join (*path_components)

def do_file (file_name, lang_codes, buildlib):
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
    translated_contents = open (file_name).read ()
    (diff_string, error) \
        = buildlib.check_translated_doc (original,
                                         file_name,
                                         translated_contents,
                                         color=use_colors and not update_mode)

    if error:
        sys.stderr.write ('warning: %s: %s' % (file_name, error))

    if update_mode:
        if error or len (diff_string) >= os.path.getsize (original):
            buildlib.read_pipe (text_editor + ' ' + file_name + ' ' + original)
        elif diff_string:
            diff_file = original + '.diff'
            f = open (diff_file, 'w')
            f.write (diff_string)
            f.close ()
            buildlib.read_pipe (text_editor + ' ' + file_name + ' ' + diff_file)
            os.remove (diff_file)
    else:
        sys.stdout.write (diff_string)

def usage ():
    sys.stdout.write (r'''
Usage:
check-translation [--language=LANG] [--verbose] [--update] FILE...

This script is licensed under the GNU GPL.
''')

def do_options ():
    global lang, verbose, update_mode, use_colors

    p = optparse.OptionParser (usage="check-translation [--language=LANG] [--verbose] FILE...",
                               description="This script is licensed under the GNU GPL.")
    p.add_option ("--language",
                  action='store',
                  default='site',
                  dest="language")
    p.add_option ("--no-color",
                  action='store_false',
                  default=True,
                  dest="color",
                  help="do not print ANSI-cooured output")
    p.add_option ("--verbose",
                  action='store_true',
                  default=False,
                  dest="verbose",
                  help="print details, including executed shell commands")
    p.add_option ('-u', "--update",
                  action='store_true',
                  default=False,
                  dest='update_mode',
                  help='call $EDITOR to update the translation')
    
    (options, files) = p.parse_args ()
    verbose = options.verbose
    lang = options.language
    use_colors = options.color
    update_mode = options.update_mode
    
    return files

def main ():
    global update_mode, text_editor

    files = do_options ()
    if 'EDITOR' in os.environ:
        text_editor = os.environ['EDITOR']
    else:
        update_mode = False
    
    buildlib.verbose = verbose

    for i in files:
        do_file (i, langdefs.LANGDICT.keys (), buildlib)

if __name__ == '__main__':
    main ()
