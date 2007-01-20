#!/usr/bin/python
import sys
import os
import os.path
#import re
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
#	if ~os.path.isdir(destdir):
#		os.mkdir(destdir)

	file_names=os.listdir(destdir)
	for file in file_names:
		if (file[-3:]=='.ly'):
			if not(file=='+.ly'):
				os.remove( os.path.join(destdir,file) )

	file_names=os.listdir(in_dir + dir)
	for file in file_names:
		out_name=file
		#out_name=re.sub(r'_',r'-',os.path.basename(file))
		#out_name=re.sub(r'\(', r'', out_name)
		#out_name=re.sub(r'\)', r'', out_name)
		#out_name=re.sub(r'\'', r'', out_name)
		src = os.path.join(srcdir, file)
		dest = os.path.join(destdir, out_name)
		#print src + "  " + dest
		shutil.copyfile(src, dest)

