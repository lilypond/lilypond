# localpackage.py
# must be included in package's python bin dir

def localpackage (n):
    p=lower (n)
    P=upper (n)
    if topdir == '':
	try:
	    topdir = os.environ[P + '_SOURCEDIR']
	except:
	    topdir = os.environ['HOME'] + '/usr/src/' + p
    sys.path.append (topdir + '/stepmake/bin')
    from packagepython import *
    package = Package (topdir)
    packager = Packager ()

