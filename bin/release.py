#!@PYTHON@

from lilypython import *
import makepatch


os.chdir(lilydirs.topdir)
os.system('make dist')
cur_ver = lilydirs.version_tuple()

os.rename('out/' + tarball(cur_ver), released_tarball(cur_ver))
os.chdir('../test')
os.system('rm ../test/*gz')
os.link(released_tarball(cur_ver), tarball(cur_ver))


makepatch.main()
os.system('gzip -9 patch*')
os.system('tar cf updeet *gz')
