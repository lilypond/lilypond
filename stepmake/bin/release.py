#!@PYTHON@
# release.py

name = 'release'
version = '0.1'

import os
import sys
import getopt

(options, files) = getopt.getopt(sys.argv[1:], 'hp:', ['help', 'package=']) 

def help ():
    sys.stdout.write ("Usage: release [OPTION]...\n"
		 "Make a tarball and patch and combined ../test/updeet\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -p, --package=DIR      specify package\n"
		      )
    sys.exit (0)

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-h' or o == '--help':
    	help ()
    elif o == '-p' or o == '--package':
	topdir = a

sys.path.append (topdir + '/stepmake/bin')
from packagepython import *
package = Package (topdir)
packager = Packager ()
from flower import *

os.chdir(package.topdir)
status =os.system('make dist')
if status:
    raise 'make dist failed'

cur_ver = package.version
# urg?
# print tarball(cur_ver)
barbaal=package.name + '-' + version_tuple_to_str (cur_ver) + '.tar.gz'
print barbaal
# os.rename('out/' + tarball(cur_ver), released_tarball(cur_ver))
# hmmm
os.remove(package.release_dir + barbaal)
os.link('out/' + barbaal, package.release_dir + barbaal)
os.chdir(package.test_dir)
os.system('set -x; rm ' + package.test_dir + package.name + '*gz')
# os.link(released_tarball(cur_ver), tarball(cur_ver))
os.link(package.release_dir + barbaal, barbaal)

# not a module, but a script (JCN)
# makepatch.main()

# Module wherefore aren't thou Script
# A Rose by any other name would be as blonde. --HWN

# urg
# os.system('@PYTHON@ ' + package.topdir + '/stepmake/bin/package-diff.py --package=' + topdir)
os.system(sys.executable + ' ' + package.topdir + '/stepmake/bin/package-diff.py --package=' + topdir)

pn = package.name + '-%s' % version_tuple_to_str(cur_ver)
pn = pn + '.diff'
pn = pn + '.gz'
rel_pn = package.patch_dir + pn

os.rename(pn, rel_pn)
os.link(rel_pn, pn)

os.system('tar cf ' + package.nickname + ' *gz')

