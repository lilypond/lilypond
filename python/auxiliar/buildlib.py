#!@PYTHON@

import subprocess
import re
import sys

verbose = False

def read_pipe (command):
    child = subprocess.Popen (command,
                              stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE,
                              shell = True)
    (output, error) = child.communicate ()
    code = str (child.wait ())
    if not child.stdout or child.stdout.close ():
        print "pipe failed: %(command)s" % locals ()
    if code != '0':
        error = code + ' ' + error
    return (output, error)

### Renamed files map to ensure continuity of file history
## Map of new_name: old_name
renames_map = {
    'usage.tely': 'user/lilypond-program.tely',
    'notation.tely': 'user/lilypond.tely',
    'learning.tely': 'user/lilypond-learning.tely',
    'changes.tely': 'topdocs/NEWS.tely',
}

# FIXME: Hardcoded file names!?
manuals_subdirectories_re = \
    re.compile ('(usage|automated-engraving|essay|extending|general|learning|notation)/')

def add_old_name (file_path):
    for new_path in renames_map:
        if file_path.endswith (new_path):
            old_file_path = file_path.replace (new_path,
                                               renames_map[new_path])
            break
    else:
        if file_path.endswith ('macros.itexi'):
            old_file_path = file_path.replace ('macros.itexi',
                                               'user/macros.itexi')
        elif file_path.endswith ('.itely'):
            old_file_path = manuals_subdirectories_re.sub ('user/',
                                                           file_path)
        elif 'snippets/' in file_path:
            old_file_path = file_path.replace ('snippets/',
                                               '../input/lsr/')
        else:
            return file_path
    return file_path + ' ' + old_file_path

revision_re = re.compile ('GIT [Cc]ommittish:\s+([a-f0-9]+)')
vc_diff_cmd = 'git diff -M %(color_flag)s %(revision)s \
%(upper_revision)s -- %(original_with_old_name)s | cat'
no_committish_fatal_error = """error: %s: no 'GIT committish: <hash>' found.
Please check the whole file against the original in English, then
fill in HEAD committish in the header.
"""

def check_translated_doc (original, translated_file, translated_contents,
                          color=False, upper_revision='HEAD'):
    m = revision_re.search (translated_contents)
    if not m:
        sys.stderr.write (no_committish_fatal_error % translated_file)
        sys.exit (1)
    revision = m.group (1)

    if color:
        color_flag = '--color --color-words'
    else:
        color_flag = '--no-color'
    original_with_old_name = add_old_name (original)
    c = vc_diff_cmd % vars ()
    if verbose:
        sys.stderr.write ('running: ' + c)
    return read_pipe (c)
