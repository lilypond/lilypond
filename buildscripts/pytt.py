#! @PYTHON@

import os
import re
import sys

frm = re.compile (sys.argv[1], re.MULTILINE)
to = sys.argv[2]

if not sys.argv[3:] or sys.argv[3] == '-':
	sys.stdout.write (re.sub (frm, to, sys.stdin.read ()))
for file in sys.argv[3:]:
	s = open (file).read ()
	name = os.path.basename (file)
	base, ext = os.path.splitext (name)
	t = re.sub (frm, to % locals (), s)
	if s != t:
		if 1:
			os.system ('mv %(file)s %(file)s~~' % locals ())
			h = open (file, "w")
			h.write (t)
			h.close ()
		else:
			sys.stdout.write (t)
