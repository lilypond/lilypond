#!@PYTHON@
# run lily, setup LaTeX input.

# Note: gettext work best if we use ' for docstrings and "
# for gettextable strings

'''
TODO:

  * check --dependencies

  * move versatile taglines, 
  
     \header {
        beginfooter=\mutopiaPD
        endfooter=\tagline  -> 'lily was here <version>'
     }

     lilytagline (->lily was here), usertagline, copyright etc.

  * head/header tagline/endfooter

  * dvi from lilypond .tex output?  This is hairy, because we create dvi
    from lilypond .tex *and* header output.

  * multiple \score blocks?
  
  * windows-sans-cygwin compatibility?  rm -rf, cp file... dir
  
'''


import os
import stat
import string
import re
import getopt
import sys
import __main__
import operator
import tempfile

datadir = '@datadir@'
sys.path.append (datadir + '/python')
try:
	import gettext
	gettext.bindtextdomain ('lilypond', '@localedir@')
	gettext.textdomain('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s


layout_fields = ['title', 'subtitle', 'subsubtitle', 'footer', 'head',
	  'composer', 'arranger', 'instrument', 'opus', 'piece', 'metre',
	  'meter', 'poet']


# init to empty; values here take precedence over values in the file 
extra_init = {
	'language' : [],
	'latexheaders' : [],
	'latexpackages' :  ['geometry'],
	'papersize' : [],
	'pagenumber' : [1],
	'textheight' : [], 
	'linewidth' : [],
	'orientation' : []
}

extra_fields = extra_init.keys ()

fields = layout_fields + extra_fields
program_name = 'ly2dvi'
help_summary = _("Generate .dvi with LaTeX for LilyPond")

include_path = ['.']
no_lily = 0
outdir = '.'
track_dependencies_p = 0
dependency_files = []

# generate ps ?
postscript_p = 0

# be verbose?
verbose_p = 0


# lily_py.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter

# BEGIN Library for these?
# cut-n-paste from ly2dvi

program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.3.142'


original_dir = os.getcwd ()
temp_dir = '%s.dir' % program_name
keep_temp_dir_p = 0
verbose_p = 0

#
# Try to cater for bad installations of LilyPond, that have
# broken TeX setup.  Just hope this doesn't hurt good TeX
# setups.  Maybe we should check if kpsewhich can find
# feta16.{afm,mf,tex,tfm}, and only set env upon failure.
#
environment = {
	'MFINPUTS' : datadir + '/mf:',
	'TEXINPUTS': datadir + '/tex:' + datadir + '/ps:.:',
	'TFMFONTS' : datadir + '/tfm:',
	'GS_FONTPATH' : datadir + '/afm:' + datadir + '/pfa',
	'GS_LIB' : datadir + '/ps',
}

def setup_environment ():
	for key in environment.keys ():
		val = environment[key]
		if os.environ.has_key (key):
			val = val + os.pathsep + os.environ[key]
		os.environ[key] = val

def identify ():
	sys.stdout.write ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
	identify ()
	sys.stdout.write ('\n')
	sys.stdout.write (_ ('Copyright (c) %s by' % ' 2001'))
	sys.stdout.write ('\n')
	sys.stdout.write ('  Han-Wen Nienhuys')
	sys.stdout.write ('  Jan Nieuwenhuizen')
	sys.stdout.write ('\n')
	sys.stdout.write (_ (r'''
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.'''))
	sys.stdout.write ('\n')

def progress (s):
	sys.stderr.write (s + '\n')

def warning (s):
	sys.stderr.write (_ ("warning: ") + s)
	sys.stderr.write ('\n')
	
		
def error (s):
	sys.stderr.write (_ ("error: ") + s)
	sys.stderr.write ('\n')
	raise _ ("Exiting ... ")

def getopt_args (opts):
	'''Construct arguments (LONG, SHORT) for getopt from  list of options.'''
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
	'''Transform one option description (4-tuple ) into neatly formatted string'''
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
	'''Convert a list of options into a neatly formatted string'''
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

def help ():
	sys.stdout.write (_ ("Usage: %s [OPTION]... FILE") % program_name)
	sys.stdout.write ('\n\n')
	sys.stdout.write (help_summary)
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Options:"))
	sys.stdout.write ('\n')
	sys.stdout.write (options_help_str (option_definitions))
	sys.stdout.write ('\n\n')
	sys.stdout.write (_ ("Report bugs to %s") % 'bug-gnu-music@gnu.org')
	sys.stdout.write ('\n')
	sys.exit (0)


def setup_temp ():
	global temp_dir
	if not keep_temp_dir_p:
		temp_dir = tempfile.mktemp (program_name)
	try:
		os.mkdir (temp_dir, 0777)
	except OSError:
		pass
	os.chdir (temp_dir)


def system (cmd, ignore_error = 0):
	if verbose_p:
		progress (_ ("Invoking `%s\'") % cmd)
	st = os.system (cmd)
	if st:
		msg =  ( _ ("error: ") + _ ("command exited with value %d") % st)
		if ignore_error:
			sys.stderr.write (msg + ' ' + _ ("(ignored)") + ' ')
		else:
			error (msg)

	return st


def cleanup_temp ():
	if not keep_temp_dir_p:
		if verbose_p:
			progress (_ ("Cleaning %s...") % temp_dir)
		system ('rm -rf %s' % temp_dir)


def set_setting (dict, key, val):
	try:
		val = string.atof (val)
	except ValueError:
		#warning (_ ("invalid value: %s") % `val`)
		pass

	try:
		dict[key].append (val)
	except KeyError:
		warning (_ ("no such setting: %s") % `key`)
		dict[key] = [val]

def strip_extension (f, ext):
	(p, e) = os.path.splitext (f)
	if e == ext:
		e = ''
	return p + e

# END Library

option_definitions = [
	('', 'h', 'help', _ ("this help")),
	('KEY=VAL', 's', 'set', _ ("change global setting KEY to VAL")),
	('DIR', 'I', 'include', _ ("add DIR to LilyPond\'s search path")),
	('', 'P', 'postscript', _ ("generate PostScript output")),
	('', 'k', 'keep', _ ("keep all output, and name the directory ly2dvi.dir")),
	('', '', 'no-lily', _ ("don't run LilyPond")),
	('', 'V', 'verbose', _ ("verbose")),
	('', 'v', 'version', _ ("print version number")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	('DIR', '', 'outdir', _ ("dump all final output into DIR")),
	('', 'd', 'dependencies', _ ("write Makefile dependencies for every input file")),
	]

def run_lilypond (files):
	opts = ''
	opts = opts + ' ' + string.join (map (lambda x : '-I ' + x, include_path))
	opts = opts + ' ' + string.join (map (lambda x : '-H ' + x, fields))

	if track_dependencies_p:
		opts = opts + " --dependencies "

	fs = string.join (files)
	
	system ('lilypond  %s %s ' % (opts, fs))

def analyse_lilypond_output (filename, extra):
	'''Grep FILENAME for interesting stuff, and
	put relevant info into EXTRA.'''
	filename = filename+'.tex'
	progress (_ ("Analyzing `%s'") % filename)
	s = open (filename).read ()

	# search only the first 10k
	s = s[:10240]
	for x in ('textheight', 'linewidth', 'papersize', 'orientation'):
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
			fname = strip_extension (fname, '.ly')
			if x:
				fname = fname + '-%d' % x

			if os.path.exists (fname + '.tex'):
				tfiles.append (find_tex_files_for_base (fname, extra))
				analyse_lilypond_output (fname, extra)
			else:
				break

			x = x + 1
	if not x:
		warning (_ ("no lilypond output found for %s") % `files`)
	return tfiles

def one_latex_definition (defn, first):
	s = '\n'
	for (k,v) in defn[1].items ():
		val = open (v).read ()
		if (string.strip (val)):
			s = s + r'''\def\lilypond%s{%s}''' % (k, val)
		else:
			s = s + r'''\let\lilypond%s\relax''' % k
		s = s + '\n'

	if first:
		s = s + '\\def\\mustmakelilypondtitle{}\n'
	else:
		s = s + '\\def\\mustmakelilypondpiecetitle{}\n'
		
	s = s + '\\input %s' % defn[0]
	return s


ly_paper_to_latexpaper =  {
	'a4' : 'a4paper',
	'letter' : 'letterpaper', 
}

def global_latex_definition (tfiles, extra):
	'''construct preamble from EXTRA,
	dump lily output files after that, and return result.
	'''


	s = ""
	s = s + '% generation tag\n'

	paper = ''

	if extra['papersize']:
		try:
			paper = '[%s]' % ly_paper_to_latexpaper[extra['papersize'][0]]
		except:
			warning (_ ("invalid value: %s") % `extra['papersize'][0]`)
			pass
	
	s = s + '\\documentclass%s{article}\n' % paper

	if extra['language']:
		s = s + r'\usepackage[%s]{babel}\n' % extra['language'][-1]


	s = s + '\\usepackage{%s}\n' \
		% string.join (extra['latexpackages'], ',')

	if extra['latexheaders']:
		s = s + '\\include{%s}\n' \
			% string.join (extra['latexheaders'], '}\n\\include{')

	textheight = ''
	if extra['textheight']:
		textheight = ',textheight=%fpt' % extra['textheight'][0]

	orientation = 'portrait'
	if extra['orientation']:
		orientation = extra['orientation'][0]

	# set sane geometry width (a4-width) for linewidth = -1.
	if not extra['linewidth'] or extra['linewidth'][0] < 0:
		linewidth = 597
	else:
		linewidth = extra['linewidth'][0]
	s = s + '\geometry{width=%spt%s,headheight=2mm,headsep=0pt,footskip=2mm,%s}\n' % (linewidth, textheight, orientation)

	s = s + r'''
\usepackage[latin1]{inputenc}
\input{titledefs}
\makeatletter
\renewcommand{\@oddfoot}{\parbox{\textwidth}{\mbox{}\thefooter}}%
'''
	
	if extra['pagenumber'] and extra['pagenumber'][-1] and extra['pagenumber'][-1] != 'no':
		s = s + r'''
\renewcommand{\@oddhead}{\parbox{\textwidth}%
    {\mbox{}\small\theheader\hfill\textbf{\thepage}}}
'''
	else:
		s = s + '\\pagestyle{empty}\n'

	s = s + '\\makeatother\n'
	s = s + '\\begin{document}\n'


	first = 1
	for t in tfiles:
		s = s + one_latex_definition (t, first)
		first = 0

	s = s + r'''
\makeatletter
\renewcommand{\@oddfoot}{\parbox{\textwidth}{\mbox{}\makelilypondtagline}}%
\makeatother
'''
	s = s + '\\end{document}'

	return s

def do_files (fs, extra):

	'''process the list of filenames in FS, using standard settings in EXTRA.
	'''
	if not no_lily:
		run_lilypond (fs)

	wfs = find_tex_files (fs, extra)
	s = global_latex_definition (wfs, extra)

	latex_file ='ly2dvi.out'
	f = open (latex_file + '.tex', 'w')
	f.write (s)
	f.close ()

	# todo: nonstopmode
	system ('latex \\\\nonstopmode \\\\input %s' % latex_file)
	return latex_file + '.dvi'

def generate_postscript (dvi_name, extra):
	'''Run dvips on DVI_NAME, optionally doing -t landscape'''

	opts = ''
	if extra['papersize']:
		opts = opts + ' -t %s' % extra['papersize'][0]

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
try:
	(options, files) = getopt.getopt(sys.argv[1:], sh, long)
except:
	help ()
	sys.exit (2)
	
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
		keep_temp_dir_p = 1
	elif o == '--no-lily':
		no_lily = 1
	elif o == '--outdir':
		outdir = a
	elif o == '--set' or o == '-s':
		ss = string.split (a, '=')
		set_setting (extra_init, ss[0], ss[1])
	elif o == '--dependencies' or o == '-d':
		track_dependencies_p = 1
	elif o == '--verbose' or o == '-V':
		verbose_p = 1
	elif o == '--version' or o == '-v':
		identify ()
		sys.exit (0)
	elif o == '--warranty' or o == '-w':
		warranty ()
		sys.exit (0)

# On most platforms, this is equivalent to
#`normpath(join(os.getcwd()), PATH)'.  *Added in Python version 1.5.2*
def compat_abspath (path):
	return os.path.normpath (os.path.join (os.getcwd (), path))

include_path = map (compat_abspath, include_path)
files = map (compat_abspath, files) 
outdir = compat_abspath (outdir)

	
files = map (lambda x: strip_extension (x, '.ly'), files)

if files:
	setup_temp ()
	setup_environment ()
	
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
	midi = base + '.midi'
	midi = os.path.join (outdir, midi)
	
	if outdir != '.':
		system ('mkdir -p %s' % outdir)
		
	#if re.match ('.*[.]dvi', string.join (os.listdir ('.'))):
	if os.path.isfile (srcname):
		# huh, and what bout all other (-1, -2) scores?
		system ('cp \"%s\" \"%s\"' % (srcname, dest))
	else:
		dest = 0
	if re.match ('.*[.]midi', string.join (os.listdir ('.'))):
		system ('cp *.midi %s' % outdir)
	else:
		midi = 0

	depfile = os.path.join (outdir, base + '.dep')

	if track_dependencies_p:
		generate_dependency_file (depfile, dest)

	os.chdir (original_dir)
	cleanup_temp ()

	# most insteresting info last
	# don't say silly things
	if os.path.isfile (depfile):
		progress (_ ("dependencies output to %s...") % depfile)
	if dest and os.path.isfile (dest):
		progress (_ ("%s output to %s...") % (type, dest))
	if midi and os.path.isfile (midi):
		progress (_ ("%s output to %s...") % ('MIDI', midi))



