#!@PYTHON@

# mf-to-table.py -- convert spacing info in  MF logs .ly and .tex
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@cs.uu.nl>

import os
import sys
import getopt
from string import *
import regex
import regsub
import time


(options, files) = getopt.getopt(
    sys.argv[1:], 'a:d:hl:o:p:t:', 
    ['afm=', 'outdir=', 'dep=', 'ly=', 'tex=', 'debug', 'help', 'package='])

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '-p' or o == '--package':
	topdir = a

sys.path.append (topdir + '/stepmake/bin')
from packagepython import *
package = Package (topdir)
packager = Packager ()

from packagepython import *
from flower import *

begin_autometric_re = regex.compile('@{')
end_autometric_re = regex.compile('@}')
include_re = regex.compile ('(\([a-zA-Z_0-9-]+\.mf\)')
autometric_re = regex.compile('@{\(.*\)@}')
version = '0.7'
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
            line = regsub.sub('\r','', line)

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

class Afm_file (File):
    def print_f_dimen(self, f):
	f = f * 1000
    
	dimstr = '%.2f' % f

	# try to mask rounding errors
	if (dimstr == '-0.00'):
		dimstr = '0.00'
	self.write( dimstr  +' ');

    def neg_print_dimen(self, str):
	self.print_f_dimen(-atof(str))
    def print_dimen(self, str):
	self.print_f_dimen(atof(str))
    def def_symbol (self, code, lily_id, tex_id, xdim, ydim):
	self.write ('C %s; N %s-%s; B ' % (code, self.groupname, lily_id))

	self.neg_print_dimen(xdim [0])
	self.neg_print_dimen(ydim [0])
	self.print_dimen(xdim [1])
	self.print_dimen(ydim [1])

	self.write (';\n');
	
    def start (self,nm):
	self.write ('Start%s\n' % nm)
    def end (self,nm):
	self.write ('End%s\n' % nm)

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
	
    def def_symbol(self, code, lily_id, tex_id, xdim, ydim):
	self.print_lit(lily_id)
	self.print_lit('\\\\' + tex_id)

	self.neg_print_dimen(xdim [0])
	self.print_dimen(xdim [1])
	self.neg_print_dimen(ydim [0])
	self.print_dimen(ydim [1])
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
	afm = self.afmfile
	if tags[0] == 'font':
	    ly.indent()
	    ly.write("% name=\\symboltables {\n")
	    self.texfile.write("% name\n")

	    afm.write ('FontName %s\n' % name)
	    afm.start ('FontMetrics')
	    afm.start ('CharMetrics')	    
	    
	elif label == "group":
	    ly.indent()
	    ly.print_lit(name)
	    ly.write(' = \\table {\n')
	    self.texfile.write("% " + name + "\n")
	    afm.groupname = name
	elif label == "puorg":
	    ly.dedent()
	    ly.write("}\n")
	    self.texfile.write("\n")
	elif label == "tnof":
	    ly.dedent()
	    ly.write("%  } % $name\n")
	    afm.end ('CharMetrics')
	    afm.end('FontMetrics');
	elif label == "char":
	    code = tags[2]
	    id = tags [7]
	    texstr = tags [8]
	    xdim = tags[3:5]
	    ydim = tags[5:7]
	    ly.def_symbol(code, id, texstr, xdim, ydim)
	    
	    self.texfile.write("\\fetdef\\%s{%s}\n" % (texstr, code))
	    afm.def_symbol (code, id, texstr, xdim, ydim)
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

    def __init__(self, lyfile_nm, texfile_nm, depfile_nm, afmfile_nm):	    
	self.lyfile = Ly_file(lyfile_nm, 'w')
	self.texfile = Indentable_file(texfile_nm, 'w')
	self.depfile = File (depfile_nm, 'w')
	self.afmfile = Afm_file (afmfile_nm, 'w')
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
		 + "  -a, --afm=FILE         .afm file\n"
		 + "  -d, --dep=FILE         print dependency info to FILE\n"
		 + "  -h, --help             print this help\n"
		 + "  -l, --ly=FILE          name output table\n"
		 + "  -o, --outdir=DIR       prefix for dependency info\n"
		 + "  -p, --package=DIR      specify package\n"
		 + "  -t, --tex=FILE         name output tex chardefs\n"
		     )
    sys.exit (0)


def main():
    identify()

    lyfile_nm = texfile_nm = '';
    depfile_nm = ''
    afmfile_nm = ''
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
	elif o=='--afm' or o == '-a':
	    afmfile_nm = a
	elif o == '--debug':
	    debug_b = 1
	elif o == '-p' or o == '--package':
	    topdir = a
	else:
	    print o
	    raise getopt.error

    log_reader = Log_reader(lyfile_nm, texfile_nm, depfile_nm, afmfile_nm)
    log_reader.outdir = outdir_prefix
    for filenm in files:
	log_reader.do_file(filenm)
    log_reader.close()


main()
