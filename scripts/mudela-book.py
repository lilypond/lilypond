#!@PYTHON@

#  TODO: center option

# log:

# 0.3:
#   rewrite in Python.

import os
import string
import re
import getopt
import sys

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
			self.file.write ('\\score { \\notes { ')

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





begin_mudela_re = re.compile ('^ *\\\\begin{mudela}')
begin_mudela_opts_re = re.compile ('^ *\\\\begin{mudela}\[(.*)\]')
end_mudela_re = re.compile ('^ *\\\\end{mudela}')
section_re = re.compile ('\\\\section')
chapter_re = re.compile ('\\\\chapter')
input_re = re.compile ('^\\\\input[ \t\n]+(.*)$')

class Tex_input:
	def __init__ (self,name):
		if not re.search ('\\.[^/\\\\]+',name):
			name = name + '.tex'
		print 'opening %s' % name
		self.filename = name
		self.infile = open (name)
		
	def get_lines (self):
		lines = self.infile.readlines ()
		(retlines, retdeps) = ([],[self.filename])
		for line in lines:
			m = input_re.search (line)
			if m:
				t = Tex_input (m.group (1))
				ls =t.get_lines ()
				retlines = retlines + ls[0]
				retdeps = retdeps + ls[1]
			else:
				retlines.append (line)

		return (retlines, retdeps)

class Main_tex_input(Tex_input):
	def __init__ (self, name, outname):
		
		Tex_input.__init__ (self, name) # ugh

		self.outname = outname
		self.chapter = 0
		self.section = 0
		self.fine_count =0
		self.mudtex = Tex_output (self.outname)
		self.mudela = None
		self.deps = []
	def set_sections (self, l):
		if section_re.search (l):
			self.section = self.section + 1
		if chapter_re.search (l):
			self.section = 0
			self.chapter = self.chapter + 1

			
	def gulp_mudela (self):
		pass

	def gen_basename (self):
		return '%s/%s-%d.%d.%d' % (outdir, self.outname,self.chapter,self.section,self.fine_count)

	def do_it(self):
		(lines, self.deps) = self.get_lines ()
		for line in lines:
			if begin_mudela_re.search (line):
				m =begin_mudela_opts_re.search (line) 
				if m:
					opts = m.group (1)
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
			elif end_mudela_re.search (line):
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
	sys.argv[1:], 'hd:o:', [ 'outdir=', 'outname=', 'help', 'dependencies'])

do_deps = 0
for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--outname' or o == '-o':
		outname = a
	if o == '--outdir' or o == '-d':
		outdir = a
	if o == '--help' or o == '-h':
		help ()
	if o == '--dependencies':
		do_deps = 1

def write_deps (fn, out,  deps):
	out_fn = outdir + '/' + fn
	print '\`writing \`%s\'\n\'' % out_fn
	
	f = open (out_fn, 'w')
	f.write ('%s: %s\n'% (outdir + '/' + out + '.dvi',
			      reduce (lambda x,y: x + ' '+ y, deps)))
	f.close ()


for f in files:
	my_outname = outname
	if not my_outname:
		my_outname = regsub.sub ('\\(.*\\)\\.doc', '\\1', f)

	my_depname = my_outname + '.dep'

	inp = Main_tex_input (f, my_outname)
	inp.do_it ()

	if do_deps:
		write_deps (my_depname, my_outname, inp.deps)


