
# python version of mudela-book.  WIP.

#  TODO: center option

# log:

# 0.3:
#   rewrite in Python.

import os
import string
import regex
import getopt
import sys
import regsub

outdir = 'out/'
program_version = '0.3'




def file_exist_b(name):
    try: 
	f = open(name)
    except IOError:
	return 0
    f.close ()
    return 1

class CompileStatus:
	pass

def file_mtime (name):
    return os.stat (name)[8] #mod time

def compile (command, infile, outfile):
	indate = file_mtime (infile)
	try:
		outdate = file_mtime (outfile)
		recompile = indate > outdate

	except os.error:
		recompile = 1

	if recompile:
		sys.stderr.write ('invoking `%s\'\n' % command)
		
		status = os.system (command)
		if status:
			raise CompileStatus


class Mudela_output:
	def __init__ (self):
		self.basename = ''
		self.fragment = 0
		self.size = 16
		
	def open (self, basename):
		self.basename = basename
		self.temp_file = "%s/%s" %(outdir, 'mudela-temp.ly')
		self.file = open (self.temp_file, 'w')
		self.file.write ('\\include \"paper%d.ly\"\n' % self.size)
		if self.size == 16:
			s = 'sixteen'
		else:
			s = 'twenty'

		self.file.write ('default_paper = \\paper { \\paper_%s\n linewidth = -15.\\cm; }\n' % s)
		
		if self.fragment:
			self.file.write ('\\score { \\melodic { ')

	def write (self,s):
		self.file.write (s)

	def close (self):
		if self.fragment:
			self.file.write (
				'}\n \\paper { linewidth = -1.0\\cm;\n' +
				'castingalgorithm = \\Wordwrap; } }\n')


		self.file.close ()

		inf=self.basename + '.ly'
		outf = self.basename + '.tex'		
		if not file_exist_b (inf):
			status = 1
		else:
#		        print 'invoking %s' %('diff %s %s' % (self.temp_file, inf))
			status = os.system ('diff -q %s %s' % (self.temp_file, inf))
#			print 'status %d' % status

		if status:
			os.rename (self.temp_file, inf)

		compile ('lilypond  -o %s %s'%  (self.basename, inf), inf, outf)
#			os.rename (self.basename + '.tex', outdir  +)
		
	

class Tex_output:
	def __init__ (self, name):
		self.output_fn = '%s/%s' % (outdir, name)
		self.mudela = 0
		self.file = open (self.output_fn , 'w')
		self.verbatim = 0		
	def open_mudela (self, basename):

		self.mudela_basename =basename
		if self.verbatim:
			self.file.write ('\\begin{verbatim}\n')
		self.mudela = 1

	def write (self, s):
		self.file.write (s)
			
	def write_mudela (self, s):
		if self.verbatim:
			self.file.write (s)
			
	def close_mudela (self):
		if self.verbatim:
			self.file.write ('\\end{verbatim}\n')
			self.verbatim = 0
		
		self.file.write (
		    '\\preMudelaExample\\input %s\n\postMudelaExample' %(self.mudela_basename))
		self.mudela = 0





begin_mudela_re = regex.compile ('^ *\\\\begin{mudela}')
begin_mudela_opts_re = regex.compile ('^ *\\\\begin{mudela}\[\(.*\)\]')
end_mudela_re = regex.compile ('^ *\\\\end{mudela}')
section_re = regex.compile ('\\\\section')
chapter_re = regex.compile ('\\\\chapter')


class Tex_input:
	def __init__ (self, name, outname):
		self.infile = open (name)
		self.outname = outname
		self.chapter = 0
		self.section = 0
		self.fine_count =0
		self.mudtex = Tex_output (self.outname)
		self.mudela = None
	def set_sections (self, l):
		if section_re.search (l) <> -1:
			self.section = self.section + 1
		if chapter_re.search (l) <> -1:
			self.section = 0
			self.chapter = self.chapter + 1

			
	def gulp_mudela (self):
		pass

	def gen_basename (self):
		return '%s/%s-%d.%d.%d' % (outdir, self.outname,self.chapter,self.section,self.fine_count)
	def do_it(self):
		lines = self.infile.readlines ()
		for line in lines:
			if begin_mudela_re.search (line) <> -1:
				if begin_mudela_opts_re.search (line) <> -1:
					opts = begin_mudela_opts_re.group (1)
				else:
					opts = ''
				optlist = string.split (opts, ',')
				self.mudela = Mudela_output ()
				if 'fragment' in optlist:
					self.mudela.fragment = 1
				if '16pt' in optlist:
					self.mudela.size = 16
				if '20pt' in optlist:
					self.mudela.size = 20
				
				if 'verbatim' in optlist:
					self.mudtex.verbatim = 1

				b = self.gen_basename ()
				self.mudtex.open_mudela (b)
				self.mudela.open (b)


				continue
			elif end_mudela_re.search (line) <> -1:
				self.mudela.close ()
				self.mudtex.close_mudela ()
				self.mudela = None
				self.fine_count = self.fine_count + 1
				continue
			    
			if self.mudela:
				self.mudela.write (line)
				self.mudtex.write_mudela (line)
			else:
				self.mudtex.write (line)
			self.set_sections(line)
		del self.mudtex
		

def help():
    sys.stdout.write("Usage: mudela-book [options] FILE\n"
		 + "Generate hybrid LaTeX input from Latex + mudela"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -d, --outdir=DIR       prefix directory\n" 
		 + "  -o, --outname=FILE     prefix for filenames\n"
		     )
    sys.exit (0)



sys.stderr.write ('This is %s version %s\n' % ('mudela-book', program_version))

outname = ''
(options, files) = getopt.getopt(
	sys.argv[1:], 'hd:o:', [ 'outdir=', 'outname=', 'help'])
for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--outname' or o == '-o':
		outname = a
	if o == '--outdir' or o == '-d':
		outdir = a
	if o == '--help' or o == '-h':
		help ()


for f in files:
    my_outname = outname
    if not my_outname:
	my_outname = regsub.sub ('\\(.*\\)\\.doc', '\\1', f)

    inp = Tex_input (f, my_outname)
    inp.do_it ()
	


