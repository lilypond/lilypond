#@PYTHON@
import os
import string
import sys

for a in sys.argv[1:]:
	# hmm, we need: text2html out/foe.txt -> out/foe.html,
	# -o is a bit overkill?
	# outfile = os.path.basename (os.path.splitext(a)[0]) + '.html'
	outfile = os.path.splitext(a)[0] + '.html'
	
	try:
	    os.unlink(outfile)
	except:
	    pass

	s = r"""
<html><body><pre>
%s
</pre></body></html>
""" % open (a).read ()
	open (outfile, 'w').write (s)


