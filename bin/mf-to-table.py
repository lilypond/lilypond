#!@PYTHON@

# 
# mf-to-table.py -- convert spacing info in  MF logs .ly and .tex
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
# 

import getopt
from string import *
import regex
import regsub
import os
import sys
import time



lilypath =''
try:
	lilypath = os.environ['LILYPOND_SOURCEDIR'] + '/'
except KeyError:
	try:
		lilypath = os.environ['top_srcdir'] + '/'
	except KeyError:
	    print 'Please set LILYPOND_SOURCEDIR to the toplevel source, eg LILYPOND_SOURCEDIR=/home/foobar/lilypond-1.2.3/'

lilypath = lilypath + '/bin/'
sys.path.append(lilypath)
 
from flower import *

begin_autometric_re = regex.compile('@{')
end_autometric_re = regex.compile('@}')
include_re = regex.compile ('(\([a-zA-Z_0-9-]+\.mf\)')
autometric_re = regex.compile('@{\(.*\)@}')
version = '0.6'
postfixes = ['log', 'dvi', '2602gf', 'tfm']

class Feta_file(File):
    """Read Feta metrics from a metafont log-file."""

    def include_scan (self, line):
	include_pos =  include_re.search (line)
	while include_pos <> -1:
	    self.dependencies.append (include_re.group (1))

	    line = line[include_pos + 1:]
	    include_pos =  include_re.search (line)

    def read_autometricline(self):
	line = ''
	while end_autometric_re.search(line) == -1 and not self.eof():
	    suf = File.readline(self)
	    self.include_scan (suf)
	    if begin_autometric_re.search(line) == -1:
		line = ''
	    line = line + regsub.sub('\n','', suf)

	if self.eof():
	   return ''

	return line;
    def readline(self):
	"""return what is enclosed in one @{ @} pair"""
	line = '';
	while autometric_re.search(line) == -1 and not self.eof():
	    line = self.read_autometricline()

	if self.eof():
	    return '';

	return autometric_re.group(1);
    def __init__(self, nm):
	File.__init__(self, nm)
	self.dependencies = []
    def do_file(infile_nm):
	infile = readline();

#
# FIXME: should parse output for {} to do indenting.
#
class Indentable_file(File):
    """Output file with support for indentation"""
    def __init__(self,nm, mode):
	File.__init__(self,nm,mode)
	self.current_indent_ = 0
	self.delta_indent_ = 4
    def writeline (self, str):
	File.write(self, str)
    def writeeol(self):
    	File.write(self, '\n')
    	File.write(self, ' '* self.current_indent_)

    def indent(self):
	self.current_indent_ = self.delta_indent_ + self.current_indent_;
    def dedent(self):
	self.current_indent_ = self.current_indent_ - self.delta_indent_;
	if self.current_indent_ < 0:
	    raise 'Nesting!'

    def write(self, str):
	lines = split(str, '\n')
	for l in lines[:-1]:
	    self.writeline(l)
	    self.writeeol()
        self.writeline (lines[-1])

class Ly_file(Indentable_file):
    """extra provisions for mozarella quirks"""
    def print_lit(self, str):
	self.write('\"%s\"\t' % str)

    def print_f_dimen(self, f):
	dimstr = '%.2f' % f

	# try to mask rounding errors
	if (dimstr == '-0.00'):
		dimstr = '0.00'
	self.write( dimstr  +'\\pt\t');

    def print_dimen(self, str):
	self.print_f_dimen(atof(str))
    
    def neg_print_dimen(self, str):
	self.print_f_dimen(-atof(str));
	
    def def_symbol(self, lily_id, tex_id, dims):
	self.print_lit(lily_id)
	self.print_lit('\\\\' + tex_id)

	self.neg_print_dimen(dims [0])
	self.print_dimen(dims [1])
	self.neg_print_dimen(dims [2])
	self.print_dimen(dims [3])
	self.write('\n')
	
	
class Log_reader:
    """Read logs, destill info, and put into output files"""
    def output_label(self, line):

	if not line:
	    return;
	tags = split(line, '@:')
	label = tags[0]
	name = tags[1]
	ly = self.lyfile	
	if tags[0] == 'font':
	    ly.indent()
	    ly.write("% name=\\symboltables {\n")
	    self.texfile.write("% name\n")
	elif label == "group":
	    ly.indent()
	    ly.print_lit(name)
	    ly.write(' = \\table {\n')
	    self.texfile.write("% " + name + "\n")
	elif label == "puorg":
	    ly.dedent()
	    ly.write("}\n")
	    self.texfile.write("\n")
	elif label == "tnof":
	    ly.dedent()
	    ly.write("%  } % $name\n")
	elif label == "char":
	    code = tags[2]
	    id = tags [7]
	    texstr = tags [8]
	    
	    ly.def_symbol(id, texstr, tags[3:7])
	    
	    self.texfile.write("\\fetdef\\%s{%s}\n" % (texstr, code))
	else:
	    raise 'unknown label: ' + label

    def writedeps (self, deps):
	if not len (deps):
	    sys.stderr.write  ('Huh, no main target??')
	    return
	filename = deps[0]
	split = os.path.splitext(filename)	
	basename=split[0];

	targets =  map (lambda x,y = basename, z = self.outdir: z + '/' + y + '.' + x, postfixes)
	depstring = reduce(lambda x,y: x + ' ' + y, deps) 
	dependencies = map (lambda x, y=depstring: x + ': ' + y, targets)
	for x in dependencies: 
	    self.depfile.write (x + '\n')
	
    def do_file(self,filenm):
	self.lyfile.write ('\n% input from ' + filenm + '\n')
	self.texfile.write ('\n% input from ' + filenm + '\n')
	feta = Feta_file(filenm)
	while not feta.eof():
	    line = feta.readline()
	    self.output_label(line)
	feta.close()
	
	self.writedeps (feta.dependencies)

    def __init__(self, lyfile_nm, texfile_nm, depfile_nm):	    
	self.lyfile = Ly_file(lyfile_nm, 'w')
	self.texfile = Indentable_file(texfile_nm, 'w')
	self.depfile = File (depfile_nm, 'w')

	headerstr = '%% Creator: %s\n%% Automatically generated on\n%% Do not edit' % \
		   (program_id() )

	self.lyfile.write(headerstr)
	self.texfile.write(headerstr)
	self.depfile.write ('# automatically generated by %s\n' % program_id ())

    def close(self):
	self.lyfile.close()
	self.texfile.close()
	self.depfile.close ()

    def __del__(self):
	self.close()

def today_str():
    return time.asctime(time.localtime(time.time()))
	
def program_id():
    return 'mf-to-table.py version ' + version;

def identify():
    sys.stdout.write(program_id() + '\n')
    
def help():
    sys.stdout.write("Usage: mf-to-table [options] LOGFILEs\n"
		 + "Generate mozarella metrics table from preparated feta log\n\n"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -l, --ly=FILE          name output table\n"
		 + "  -d, --dep=FILE         print dependency info to FILE\n"
		 + "  -o, --outdir=DIR       prefix for dependency info\n"
		 + "  -t, --tex=FILE         name output tex chardefs\n")
    sys.exit (0)


def main():
    identify()
    (options, files) = getopt.getopt(
	sys.argv[1:], 'ho:l:t:d:', ['outdir=', 'dep=', 'ly=', 'tex=', 'debug', 'help'])

    lyfile_nm = texfile_nm = '';
    depfile_nm = ''
    outdir_prefix = '.'
    for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--dep' or o == '-d':
	    depfile_nm = a
	elif o == '--outdir' or o == '-o':
	    outdir_prefix = a
	elif o == '--ly' or o == '-l':
	    lyfile_nm = a
	elif o == '--tex' or o == '-t':
	    texfile_nm = a
	elif o== '--help' or o == '-h':
	    help()
	elif o == '--debug':
	    debug_b = 1
	else:
	    print o
	    raise getopt.error

    log_reader = Log_reader(lyfile_nm, texfile_nm, depfile_nm)
    log_reader.outdir = outdir_prefix
    for filenm in files:
	log_reader.do_file(filenm)
    log_reader.close()


main()
