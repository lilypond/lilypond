
import re
import sys


vf = 'VERSION'
if sys.argv[1:]:
	vf = sys.argv[1]

f = open (vf)
ls = f.readlines ()
mypatch = 0
defs = []
for l in ls:
	l = re.sub ('#.*','', l)
	m  = re.search ('([^ =]*)[\t ]*=[ \t]*([^ \t]*)[ \t]*\n', l)
	if m:
		defs.append ((m.group(1), m.group(2)))


sys.stdout.write ('/* automatically generated */')
for d in defs:
	sys.stdout.write ('#define %s \"%s\"\n' % d)

if ('MY_PATCH_LEVEL', '') in defs:
	sys.stdout.write ('#define NO_MY_PATCHLEVEL')

sys.stdout.write('\n');
	
