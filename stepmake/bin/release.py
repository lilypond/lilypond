#!@PYTHON@
# release.py

import os
import packagepython
import getopt
import string
import sys
import time

(options, files) = getopt.getopt (sys.argv[1:], 'ho:p:',
                 ['help', 'outdir=', 'package='])

def help ():
    sys.stdout.write (r"""Usage: release [OPTIONS]...
Make a tarball and patch.

Options:
 -o, --outdir=DIR       specify where to leave patches
 -h, --help                 print this help
 -p, --package=DIR      specify package"""
)
    sys.exit (0)


topdir = ''
outdir = '.'

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-h' or o == '--help':
        help ()
    elif o == '-p' or o == '--package':
        topdir = a
    elif o == '--outdir' or o == '-o':
        outdir = a
      
sys.path.append (topdir + '/stepmake/bin')

package = packagepython.Package (topdir)
os.chdir (package.topdir)

release_version = packagepython.version_tuple_to_str (package.version)
basename = string.join ((package.name, release_version), '-')
tarball = basename + '.tar.gz'
out_tarfile = os.path.join (outdir, tarball)
release_tarfile = os.path.join (package.release_dir, tarball)

if os.path.exists (out_tarfile):
    os.unlink (out_tarfile)

changelog_name = os.path.join (topdir, 'ChangeLog')
lines = open (changelog_name).readlines ()
release_marker = '\t* VERSION: %(release_version)s' % vars ()
if not package.version[3] \
 and lines[2][0:len (release_marker) - 1] != release_marker:
    sys.stderr.write ("warning: ChangeLog: adding VERSION: %s\n" \
             % release_version)
    user_changelog_entry = time.strftime ('%Y-%m-%d') \
               + '  ' + os.environ['EMAIL']
    changelog = open (changelog_name, 'w')
    changelog.write (user_changelog_entry)
    changelog.write ('\n\n')
    changelog.write (release_marker)
    changelog.write ('\n\n')
    changelog.writelines (lines)
    changelog.close ()

status = os.system ('make dist')
if status:
    raise 'make dist failed'

if os.path.exists (release_tarfile):
    os.unlink (release_tarfile)
    
os.link (out_tarfile, release_tarfile)

diff_py = package.topdir + '/stepmake/bin/package-diff.py'
diff_py_options = '--outdir=%(outdir)s --package=%(topdir)s' % vars ()
status = os.system (string.join ((sys.executable, diff_py, diff_py_options)))
if status:
    raise 'make diff failed'

previous_tuple = packagepython.prev_version (package.version)
previous_version = packagepython.version_tuple_to_str (previous_tuple)

diff_base = string.join ((package.name, previous_version, release_version),
            '-')
diff_name = diff_base + '.diff.gz'

out_diff = os.path.join (outdir, diff_name)
release_diff = os.path.join (package.patch_dir, diff_name)

if not os.path.exists (out_diff):
    sys.stderr.write ("error: cannot open: %s\n" % out_diff)
    sys.exit (1)

if os.path.exists (release_diff):
    os.unlink (release_diff)

os.link (out_diff, release_diff)

