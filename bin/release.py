#!@PYTHON@

import sys
import os

lilypath =''
try:
    lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'
    sys.exit(1)

lilypath = lilypath + '/bin/'
sys.path.append(lilypath)

from lilypython import *

os.chdir(lilydirs.topdir)
os.system('make dist')
cur_ver = lilydirs.version_tuple()
print tarball(cur_ver)
os.rename('out/' + tarball(cur_ver), released_tarball(cur_ver))
os.chdir('../test')
os.system('pwd')
os.system('rm ../test/*gz')
os.link(released_tarball(cur_ver), tarball(cur_ver))


# not a module, but a script (JCN)
# makepatch.main()

# Module wherefore aren't thou Script
# A Rose by any other name would be as blonde. --HWN

os.system('python ' + lilydirs.topdir + '/bin/make-patch.py');

pn = 'patch-%s' % version_tuple_to_str(cur_ver)
os.system('gzip -9 ' + pn)
pn = pn + '.gz'
rel_pn = lilydirs.release_dir + '../patches/' + pn;

os.rename(pn, rel_pn);
os.link(rel_pn, pn);

os.system('tar cf updeet *gz')
