#!/usr/bin/python
import sys
import os
import re
import string

if len (sys.argv) < 5:
	print 'args: LOGFILE CLASS FUNC NEW_FUNC'
	
func = sys.argv[3]
new_func = sys.argv[4]
klazz = sys.argv[2]
log_ls = open (sys.argv[1]).readlines ()
regex = re.compile ("([^:]+):([0-9]+): error: .class ([_a-zA-Z]+). has no member named .%s." % func)

files = {}

for l in log_ls:
	m =  regex.search (l)
	if not m:
		continue
	print l

	file = m.group (1)
	line_no = string.atoi (m.group (2))
	klass = m.group (3)
	
	if klass <> klazz:
		continue

	if not files.has_key (file):
		files[file] = open (file).read ().split ('\n')

	line_no -= 1 
	files[file][line_no] = re.sub (func, new_func, files[file][line_no])


for (f,ls) in files.items():
	print 'writing ', f 
	os.rename (f, f + '~')
	open (f, 'w').write ('\n'.join (ls))

