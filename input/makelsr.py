#!/usr/bin/python
import sys
import os
import os.path
import shutil

dirs=['advanced','trick']

try:
	in_dir=sys.argv[1]
except:
	print "Please specify input_file."
	exit

for dir in dirs:
	srcdir = os.path.join(in_dir, dir)
	destdir = os.path.join(os.getcwd(), 'lsr', dir)

	file_names=os.listdir(destdir)
	for file in file_names:
		if (file[-3:]=='.ly'):
			if not(file=='+.ly'):
				os.remove( os.path.join(destdir,file) )

	file_names=os.listdir(in_dir + dir)
	for file in file_names:
		out_name=file
		src = os.path.join(srcdir, file)
		dest = os.path.join(destdir, out_name)
		shutil.copyfile(src, dest)

