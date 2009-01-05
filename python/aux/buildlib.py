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

revision_re = re.compile ('GIT [Cc]ommittish:\s+([a-f0-9]+)')
vc_diff_cmd = 'git diff %(color_flag)s %(revision)s HEAD -- %(original)s | cat'

def check_translated_doc (original, translated_file, translated_contents, color=False):
    m = revision_re.search (translated_contents)
    if not m:
        sys.stderr.write ('error: ' + translated_file + \
                          ": no 'GIT committish: <hash>' found.\nPlease check " + \
                          'the whole file against the original in English, then ' + \
                          'fill in HEAD committish in the header.\n')
        sys.exit (1)
    revision = m.group (1)

    if color:
        color_flag = '--color'
    else:
        color_flag = '--no-color'
    c = vc_diff_cmd % vars ()
    if verbose:
        sys.stderr.write ('running: ' + c)
    return read_pipe (c)
