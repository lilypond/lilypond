#!@PYTHON@
# release.py

import os
import sys
import getopt

topdir = ''
outdir = '.'

(options, files) = getopt.getopt(sys.argv[1:], 'ho:p:', ['help', 'outdir=', 'package=']) 

def help ():
	sys.stdout.write (r"""Usage: release [OPTION]...
Make a tarball and patch

Options:
  -o, --outdir=DIR       where to leave patches.
  -h, --help             print this help
  -p, --package=DIR      specify package"""
)
	sys.exit (0)

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
from packagepython import *
package = Package (topdir)
os.chdir(package.topdir)

try:
	os.system ('set -x; rm ' + os.path.join (outdir, package.name + '*gz'))
except:
	pass



status = os.system('make dist')
if status:
	raise 'make dist failed'

cur_ver = package.version


pn = '%s-%s' % (package.name, version_tuple_to_str (cur_ver))
tarball = pn + '.tar.gz'
orig  = os.path.join (outdir, tarball)
try:
	os.remove(os.path.join (package.release_dir, tarball))
except:
	pass
os.link(orig,  os.path.join (package.release_dir, tarball))

# urg: howto check exit code?
os.system(sys.executable + ' ' + package.topdir + '/stepmake/bin/package-diff.py --outdir=%s --package=%s' % (outdir, topdir))

prev_ver = prev_version (cur_ver)
dn = '%s-%s-%s' % (package.name, version_tuple_to_str (prev_ver),
		   version_tuple_to_str (cur_ver))
diffname = dn + '.diff.gz'
rel_pn = package.patch_dir + diffname

diffname = os.path.join (outdir, diffname)

try:
	os.rename(diffname, rel_pn)
except:
	sys.stderr.write ("Can't find diff: %s\n" % diffname)
	sys.exit (1)
os.link(rel_pn, diffname)

