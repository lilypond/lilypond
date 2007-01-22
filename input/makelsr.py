#!/usr/bin/python
import sys
import os
import os.path
import shutil

dirs = ['advanced','trick']

try:
	in_dir = sys.argv[1]
except:
	print "Please specify input_file."
	exit

for dir in dirs:
	srcdir = os.path.join (in_dir, dir)
	destdir = os.path.join (os.getcwd(), 'lsr', dir)

	file_names = os.listdir (destdir)
	for file in file_names:
		if (file.endswith ('.ly')):
			if (file[:3] != 'AAA'):   # or whatever we use to denote the first file
				os.remove( os.path.join(destdir,file) )

	file_names = os.listdir (in_dir + dir)
	for file in file_names:
		src = os.path.join (srcdir, file)
		dest = os.path.join (destdir, file)
		shutil.copyfile (src, dest)

