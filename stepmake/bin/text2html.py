#@PYTHON@
import os
import string
import sys

for a in sys.argv[1:]:
	outfile = os.path.basename (os.path.splitext(a)[0]) + '.html'
	
	try:
	    os.unlink(outfile)
	except:
	    pass

	s = r"""
<body>
<xmp>%s
</xmp>
</body>""" % open (a).read ()
	open (outfile, 'w').write (s)


