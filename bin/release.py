#!@PYTHON@

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


# not a module, but a script:
# makepatch.main()
os.system('python ' + lilydirs.topdir + '/bin/make-patch.py');

os.system('gzip -9 patch*')
os.system('tar cf updeet *gz')
