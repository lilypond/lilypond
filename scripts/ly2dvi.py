#!@PYTHON@
# run lily, setup LaTeX input.

""" TODO: --dependencies

"""


import os
import stat
import string
import re
import getopt
import sys
import __main__
import operator
import tempfile


layout_fields = ['title', 'subtitle', 'subsubtitle', 'footer', 'head',
	  'composer', 'arranger', 'instrument', 'opus', 'piece', 'metre',
	  'meter', 'poet']


# init to empty; values here take precedence over values in the file 
extra_init = {
	'language' : [],
	'latexheaders' : [],
	'latexpackages' :  ['geometry'],
	'papersizename' : [],
	'pagenumber' : [],
	'textheight' : [], 
	'linewidth' : [],
	'orientation' : []
}

extra_fields = extra_init.keys ()

fields = layout_fields + extra_fields
original_dir = os.getcwd ()
include_path = ['.']
temp_dir = ''
keep_temp_dir = 0
no_lily = 0
outdir = '.'
track_dependencies_p = 0

dependency_files = []


program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.3.134'

# generate ps ?
postscript_p = 0

option_definitions = [
	('', 'h', 'help', 'print help'),
	('KEY=VAL', 's', 'set', 'change global setting KEY to VAL'),	
	('', 'P', 'postscript', 'Generate PostScript output'),
	('', 'k', 'keep', 'Keep all output, and name the directory ly2dvi.dir'),
	('', '', 'no-lily', 'Don\'t run lilypond'),
	('', 'v', 'version', "Print version and copyright info"),
	('DIR', '', 'outdir', 'Dump all final output into DIR'),
	('', 'd', 'dependencies', 'Dump all final output into DIR'),
	]



def identify():
	sys.stdout.write ('lilypond-book (GNU LilyPond) %s\n' % program_version)

def print_version ():
	identify()
	sys.stdout.write (r"""Copyright 1998--1999
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.""")



def progress (s):
	"""Make the progress messages stand out between lilypond stuff"""
	sys.stderr.write (' *** ' + s+ '\n')
	
def error (s):
	sys.stderr.write (s)
	raise 'Exiting ... '


def find_file (name):
	"""
	Search the include path for NAME. If found, return the (CONTENTS, PATH) of the file.
	"""
	
	f = None
	nm = ''
	for a in include_path:
		try:
			nm = os.path.join (a, name)
			f = open (nm)
			__main__.read_files.append (nm)
			break
		except IOError:
			pass
	if f:
		sys.stderr.write ("Reading `%s'\n" % nm)
		return (f.read (), nm)
	else:
		error ("File not found `%s'\n" % name)
		return ('', '')




def getopt_args (opts):
	"Construct arguments (LONG, SHORT) for getopt from  list of options."
	short = ''
	long = []
	for o in opts:
		if o[1]:
			short = short + o[1]
			if o[0]:
				short = short + ':'
		if o[2]:
			l = o[2]
			if o[0]:
				l = l + '='
			long.append (l)
	return (short, long)

def option_help_str (o):
	"Transform one option description (4-tuple ) into neatly formatted string"
	sh = '  '	
	if o[1]:
		sh = '-%s' % o[1]

	sep = ' '
	if o[1] and o[2]:
		sep = ','
		
	long = ''
	if o[2]:
		long= '--%s' % o[2]

	arg = ''
	if o[0]:
		if o[2]:
			arg = '='
		arg = arg + o[0]
	return '  ' + sh + sep + long + arg


def options_help_str (opts):
	"Convert a list of options into a neatly formatted string"
	w = 0
	strs =[]
	helps = []

	for o in opts:
		s = option_help_str (o)
		strs.append ((s, o[3]))
		if len (s) > w:
			w = len (s)

	str = ''
	for s in strs:
		str = str + '%s%s%s\n' % (s[0], ' ' * (w - len(s[0])  + 3), s[1])
	return str

def help():
	sys.stdout.write("""Usage: lyvi [options] FILE\n
Generate .dvi with LaTeX for lilypond
Options:
""")
	sys.stdout.write (options_help_str (option_definitions))
	sys.stdout.write (r"""Warning all output is written in the CURRENT directory



Report bugs to bug-gnu-music@gnu.org.

Written by
Han-Wen Nienhuys <hanwen@cs.uu.nl>
""")

	sys.exit (0)


def setup_temp ():
	global temp_dir
	temp_dir = 'ly2dvi.dir'
	if not keep_temp_dir:
		temp_dir = tempfile.mktemp ('ly2dvi')
		
	try:
		os.mkdir (temp_dir)
	except OSError:
		pass
		

	# try not to gen/search MF stuff in temp dir
	fp = ''
	try:
		fp = ':' + os.environ['TFMFONTS']
	except KeyError:
		fp = '://:'

		
	os.environ['TFMFONTS'] =  original_dir + fp

	os.chdir (temp_dir)
	progress ('Temp directory is `%s\'\n' % temp_dir) 

	
def system (cmd, ignore_error = 0):
	sys.stderr.write ("invoking `%s\'\n" % cmd)
	st = os.system (cmd)
	if st:
		msg =  ('Error command exited with value %d' % st)
		if ignore_error:
			sys.stderr.write (msg + ' (ignored)\n')
		else:
			error (msg)

	return st

def cleanup_temp ():
	if not keep_temp_dir:
		progress ('Cleaning up `%s\'' % temp_dir)
		system ('rm -rf %s' % temp_dir)
	

def run_lilypond (files):
	opts = ''
	opts = opts + ' ' + string.join (map (lambda x : '-I ' + x, include_path))
	opts = opts + ' ' + string.join (map (lambda x : '-H ' + x, fields))

	if track_dependencies_p:
		opts = opts + " --dependencies "

	fs = string.join (files)
	
	system ('lilypond  %s %s ' % (opts, fs))


def set_setting (dict, key, val):
	try:
		val = string.atof (val)
	except ValueError:
		pass

	try:
		dict[key].append (val)
	except KeyError:
		dict[key] = [val]
	

def analyse_lilypond_output (filename, extra):
	"""Grep FILENAME for interesting stuff, and
	put relevant info into EXTRA."""
	filename = filename+'.tex'
	progress ("Analyzing `%s'" % filename)
	s = open (filename).read ()

	# search only the first 10k
	s = s[:10240]
	for x in ('textheight', 'linewidth', 'papersizename', 'orientation'):
		m = re.search (r'\\def\\lilypondpaper%s{([^}]*)}'%x, s)
		if m:
			set_setting (extra, x, m.group (1))

def find_tex_files_for_base (base, extra):
	headerfiles = {}
	for f in layout_fields:
		if os.path.exists (base + '.' + f):
			headerfiles[f] = base+'.'+f

	if os.path.exists (base  +'.dep'):
		dependency_files.append (base + '.dep')

	for f in extra_fields:
		if os.path.exists (base + '.' + f):
			extra[f].append (open (base + '.' + f).read ())
	
	return (base  +'.tex',headerfiles)
	 

def find_tex_files (files, extra):
	tfiles = []
	for f in files:
		x = 0
		while 1:
			fname = os.path.basename (f)
			fname = os.path.splitext (fname)[0]
			if x:
				fname = fname + '-%d' % x

			if os.path.exists (fname + '.tex'):
				tfiles.append (find_tex_files_for_base (fname, extra))
				analyse_lilypond_output (fname, extra)
			else:
				break

			x = x +1 
	return tfiles

def one_latex_definition (defn, first):
	s = ''
	for (k,v) in defn[1].items ():
		s = r"""\def\the%s{%s}""" % (k,open (v).read ())

	if first:
		s = s + '\\makelilytitle\n'
	else:
		s = s + '\\makelilypiecetitle\n'
		
	s = s + '\\input %s' % defn[0]
	return s


ly_paper_to_latexpaper =  {
	'a4' : 'a4paper',
	
}

def global_latex_definition (tfiles, extra):
	"""construct preamble from EXTRA,
	dump lily output files after that, and return result.
	"""


	s = ""
	s = s + '% generation tag\n'

	paper = ''

	if extra['papersizename']:
		paper = '[%s]' % ly_paper_to_latexpaper[extra['papersizename'][0]]
	s = s + '\\documentclass%s{article}\n' % paper

	if extra['language']:
		s = s + r'\usepackage[%s]{babel}\n' % extra['language'][-1]


	s = s + '\\usepackage{%s}\n' \
		% string.join (extra['latexpackages'], ',')
	
	s = s + string.join (extra['latexheaders'], ' ')

	textheight = ''
	if extra['textheight']:
		textheight = ',textheight=%fpt' % extra['textheight'][0]

	orientation = 'portrait'
	if extra['orientation']:
		orientation = extra['orientation'][0]
 
	s = s + '\geometry{width=%spt%s,headheight=2mm,headsep=0pt,footskip=2mm,%s}\n' % (extra['linewidth'][0], textheight, orientation)

	s= s + r"""
\usepackage[latin1]{inputenc} 
\input{titledefs}
\makeatletter
\renewcommand{\@oddfoot}{\parbox{\textwidth}{\mbox{}\thefooter}}%%
"""
	if extra['pagenumber'] and  extra['pagenumber'][-1]:
		s = s + r"""
		\renewcommand{\@oddhead}{\parbox{\textwidth}%%
		{\mbox{}\small\theheader\hfill\textbf{\thepage}}}%%"""
	else:
		s = s + '\\pagestyle{empty}'
		
	s = s + '\\begin{document}'

	first = 1
	for t in tfiles:
		s = s + one_latex_definition (t, first)
		first = 0
		
	s = s + '\\end{document}'

	return s

def do_files (fs, extra):

	"""process the list of filenames in FS, using standard settings in EXTRA.
	"""
	if not no_lily:
		run_lilypond (fs)

	wfs = find_tex_files (fs, extra)
	s = global_latex_definition (wfs, extra)

	latex_file ='ly2dvi.out'
	f = open (latex_file + '.tex', 'w')
	f.write (s)
	f.close ()

	# todo: nonstopmode
	system ('latex %s' % latex_file)
	return latex_file + '.dvi'

def generate_postscript (dvi_name, extra):
	"""Run dvips on DVI_NAME, optionally doing -t landscape"""

	opts = ''
	if extra['papersizename']:
		opts = opts + ' -t %s' % extra['papersizename'][0]

	if extra['orientation'] and extra['orientation'][0] == 'landscape':
		opts = opts + ' -t landscape'

	ps_name = re.sub (r'\.dvi', r'.ps', dvi_name)
	system ('dvips %s -o %s %s' % (opts, ps_name, dvi_name))

	return ps_name
		


def generate_dependency_file (depfile, outname):
	df = open (depfile, 'w')
	df.write (outname + ':' )
	
	for d in dependency_files:
		s = open (d).read ()
		s = re.sub ('#[^\n]*\n', '', s)
		s = re.sub (r'\\\n', ' ', s)
		m = re.search ('.*:(.*)\n', s)

		# ugh. Different targets?
		if m:
			df.write ( m.group (1)  + ' ' )

	df.write ('\n')
	df.close ();

(sh, long) = getopt_args (__main__.option_definitions)
(options, files) = getopt.getopt(sys.argv[1:], sh, long)
for opt in options:	
	o = opt[0]
	a = opt[1]

	if 0:
		pass
	elif o == '--help' or o == '-h':
		help ()
	elif o == '--include' or o == '-I':
		include_path.append (a)
	elif o == '--postscript' or o == '-P':
		postscript_p = 1
	elif o == '--keep' or o == '-k':
		keep_temp_dir = 1
	elif o == '--no-lily':
		no_lily = 1
	elif o == '--outdir':
		outdir = a
	elif o == '--set' or o == '-s':
		ss = string.split (a, '=')
		set_setting (extra_init, ss[0], ss[1])
	elif o == '--dependencies' or o == '-d':
		track_dependencies_p = 1
	elif o == '--version' or o == '-v':
		identify ()
		
		
include_path = map (os.path.abspath, include_path)
files = map (os.path.abspath, files) 
outdir = os.path.abspath (outdir)

def strip_ly_suffix (f):
	(p, e) =os.path.splitext (f)
	if e == '.ly':
		e = ''
	return p +e
	
files = map (strip_ly_suffix, files)

if files:
	setup_temp ()
	extra = extra_init
	
	dvi_name = do_files (files, extra)

	if postscript_p:
		ps_name = generate_postscript (dvi_name, extra)



	base = os.path.basename (files[0])
	dest = base
	type = 'foobar'
	srcname = 'foobar'
	
	if postscript_p:
		srcname = ps_name
		dest = dest + '.ps'
		type = 'PS'
	else:
		srcname = dvi_name
		dest= dest + '.dvi'
		type = 'DVI'

	dest = os.path.join (outdir, dest)
	system ('cp \"%s\" \"%s\"' % (srcname, dest ))
	system ('cp *.midi %s' % outdir, ignore_error = 1)

	progress ("%s file left in `%s'\n" % (type, dest))

	depfile = os.path.join (outdir, base + '.dep')

	if track_dependencies_p:
		generate_dependency_file (depfile, dest)
	progress ("Dependency file left in `%s'\n" % depfile)

	cleanup_temp ()


