#!@PYTHON@
# Run lilypond, latex, dvips.
#
# This is the third incarnation of ly2dvi.
#
# Earlier incarnations of ly2dvi were written by
# Jeffrey B. Reed<daboys@austin.rr.com> (Python version)
# Jan Arne Fagertun <Jan.A.Fagertun@@energy.sintef.no> (Bourne shell script)
#


# 
# TODO: should allow to set a central pk cache directory from the command line.
# TODO: should allow to switch off pk cache.
#


# Note: gettext work best if we use ' for docstrings and "
#       for gettextable strings.
#       --> DO NOT USE """ for docstrings.

'''
TODO:

  * figure out which set of command line options should make ly2dvi:

      na: create tex only?  
      na: create latex only? 
      na: create tex and latex
      default: create dvi only
      na: create tex, latex and dvi
      -P: create dvi and ps
      na: * create ps only

     etc.

     for foo.ly, rename ly2dvi.dir to out-ly2dvi, foo.ly2dvi, foo.dir ?
     
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
  
'''





import os
import stat
import string
import re
import getopt
import sys
import shutil
import __main__
import operator
import tempfile
import traceback

# Handle bug in Python 1.6-2.1
#
# there are recursion limits for some patterns in Python 1.6 til 2.1. 
# fix this by importing pre instead. Fix by Mats.

# todo: should check Python version first.
try:
	import pre
	re = pre
	del pre
except ImportError:
	import re


################################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter

try:
	import gettext
	gettext.bindtextdomain ('lilypond', localedir)
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.5.17'

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
	errorport.write (s + '\n')

def warning (s):
	progress (_ ("warning: ") + s)

def user_error (s, e=1):
	errorport.write (program_name + ":" + _ ("error: ") + s + '\n')
	sys.exit (e)
	
def error (s):


	'''Report the error S.  Exit by raising an exception. Please
	do not abuse by trying to catch this error. If you do not want
	a stack trace, write to the output directly.

	RETURN VALUE

	None
	
	'''
	
	progress (_ ("error: ") + s)
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
	ls = [(_ ("Usage: %s [OPTION]... FILE") % program_name),
		('\n\n'),
		(help_summary),
		('\n\n'),
		(_ ("Options:")),
		('\n'),
		(options_help_str (option_definitions)),
		('\n\n'),
		(_ ("Report bugs to %s") % 'bug-lilypond@gnu.org'),
		('\n')]
	map (sys.stdout.write, ls)
	
def setup_temp ():
	"""
	Create a temporary directory, and return its name. 
	"""
	global temp_dir
	if not keep_temp_dir_p:
		temp_dir = tempfile.mktemp (program_name)
	try:
		os.mkdir (temp_dir, 0777)
	except OSError:
		pass

	return temp_dir


def system (cmd, ignore_error = 0):
	"""Run CMD. If IGNORE_ERROR is set, don't complain when CMD returns non zero.

	RETURN VALUE

	Exit status of CMD
	"""
	
	if verbose_p:
		progress (_ ("Invoking `%s\'") % cmd)
	st = os.system (cmd)
	if st:
		name = re.match ('[ \t]*([^ \t]*)', cmd).group (1)
		msg = name + ': ' + _ ("command exited with value %d") % st
		if ignore_error:
			warning (msg + ' ' + _ ("(ignored)") + ' ')
		else:
			error (msg)

	return st


def cleanup_temp ():
	if not keep_temp_dir_p:
		if verbose_p:
			progress (_ ("Cleaning %s...") % temp_dir)
		shutil.rmtree (temp_dir)


def strip_extension (f, ext):
	(p, e) = os.path.splitext (f)
	if e == ext:
		e = ''
	return p + e

################################################################
# END Library






# if set, LILYPONDPREFIX must take prevalence
# if datadir is not set, we're doing a build and LILYPONDPREFIX 
datadir = '@datadir@'


if os.environ.has_key ('LILYPONDPREFIX') :
# huh ? this always leads to exception.
# or '@datadir@' == '@' + 'datadir' + '@':   
	datadir = os.environ['LILYPONDPREFIX']
else:
	datadir = '@datadir@'


while datadir[-1] == os.sep:
	datadir= datadir[:-1]

program_name = 'ly2dvi'

original_dir = os.getcwd ()
temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)
errorport = sys.stderr
keep_temp_dir_p = 0
verbose_p = 0

try:
	import gettext
	gettext.bindtextdomain ('lilypond', '@localedir@')
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

# Attempt to fix problems with limited stack size set by Python!
# Sets unlimited stack size. Note that the resource module only
# is available on UNIX.
try:
       import resource
       resource.setrlimit (resource.RLIMIT_STACK, (-1, -1))
except:
       pass

help_summary = _ ("Generate .dvi with LaTeX for LilyPond")

option_definitions = [
	('', 'd', 'dependencies', _ ("write Makefile dependencies for every input file")),
	('', 'h', 'help', _ ("this help")),
	(_ ("DIR"), 'I', 'include', _ ("add DIR to LilyPond's search path")),
	('', 'k', 'keep', _ ("keep all output, and name the directory %s.dir") % program_name),
	('', '', 'no-lily', _ ("don't run LilyPond")),
	('', 'm', 'no-paper', _ ("produce MIDI output only")),
	(_ ("FILE"), 'o', 'output', _ ("write ouput to FILE")),
	(_ ("FILE"), 'f', 'find-pfa', _ ("find pfa fonts used in FILE")),
	# why capital P?
	('', 'P', 'postscript', _ ("generate PostScript output")),
	(_ ("KEY=VAL"), 's', 'set', _ ("change global setting KEY to VAL")),
	('', 'V', 'verbose', _ ("verbose")),
	('', 'v', 'version', _ ("print version number")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

layout_fields = ['dedication', 'title', 'subtitle', 'subsubtitle',
	  'footer', 'head', 'composer', 'arranger', 'instrument',
	  'opus', 'piece', 'metre', 'meter', 'poet', 'texttranslator']


# init to empty; values here take precedence over values in the file

## TODO: change name.
extra_init = {
	'language' : [],
	'latexheaders' : [],
	'latexpackages' :  ['geometry'],
	'latexoptions' : [],
	'papersize' : [],
	'pagenumber' : [1],
	'textheight' : [], 
	'linewidth' : [],
	'orientation' : []
}

extra_fields = extra_init.keys ()
fields = layout_fields + extra_fields

include_path = ['.']
lily_p = 1
paper_p = 1
cache_pks_p = 1

PK_PATTERN='feta.*\.[0-9]+pk'

output_name = ''
targets = {
	'DVI' : 0,
	'LATEX' : 0,
	'MIDI' : 0,
	'TEX' : 0,
	}

track_dependencies_p = 0
dependency_files = []


#
# Try to cater for bad installations of LilyPond, that have
# broken TeX setup.  Just hope this doesn't hurt good TeX
# setups.  Maybe we should check if kpsewhich can find
# feta16.{afm,mf,tex,tfm}, and only set env upon failure.
#
environment = {
	'MFINPUTS' : datadir + '/mf' + ':',
	'TEXINPUTS': datadir + '/tex:' + datadir + '/ps:' + '.:'
		+ os.getcwd() + ':',
	'TFMFONTS' : datadir + '/tfm' + ':',
	'GS_FONTPATH' : datadir + '/afm:' + datadir + '/pfa',
	'GS_LIB' : datadir + '/ps',
}


def setup_environment ():
	for key in environment.keys ():
		val = environment[key]
		if os.environ.has_key (key):
			val = os.environ[key] + os.pathsep + val 
		os.environ[key] = val

#what a name.
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


def print_environment ():
	for (k,v) in os.environ.items ():
		sys.stderr.write ("%s=\"%s\"\n" % (k,v)) 

def run_lilypond (files, outbase, dep_prefix):
	opts = ''
#	opts = opts + '--output=%s.tex' % outbase
	opts = opts + ' ' + string.join (map (lambda x : '-I ' + x,
					      include_path))
	if paper_p:
		opts = opts + ' ' + string.join (map (lambda x : '-H ' + x,
						      fields))
	else:
		opts = opts + ' --no-paper'
		
	if track_dependencies_p:
		opts = opts + " --dependencies"
		if dep_prefix:
			opts = opts + ' --dep-prefix=%s' % dep_prefix

	fs = string.join (files)

	if not verbose_p:
		# cmd = cmd + ' 1> /dev/null 2> /dev/null'
		progress ( _("Running %s...") % 'LilyPond')
	else:
		opts = opts + ' --verbose'

		# for better debugging!
		print_environment ()
	print opts, fs	
	system ('lilypond %s %s ' % (opts, fs))

def analyse_lilypond_output (filename, extra):
	
	# urg
	'''Grep FILENAME for interesting stuff, and
	put relevant info into EXTRA.'''
	filename = filename+'.tex'
	progress (_ ("Analyzing %s...") % filename)
	s = open (filename).read ()

	# search only the first 10k
	s = s[:10240]
	for x in extra_fields:
		m = re.search (r'\\def\\lilypondpaper%s{([^}]*)}'%x, s)
		if m:
			set_setting (extra, x, m.group (1))

def find_tex_files_for_base (base, extra):

	"""
	Find the \header fields dumped from BASE.
	"""
	
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
	"""
	Find all .tex files whose prefixes start with some name in FILES. 

	"""
	
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
		fstr = string.join (files, ', ')
		warning (_ ("no lilypond output found for %s") % fstr)
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
		
	s = s + '\\input %s\n' % defn[0] # The final \n seems important here. It ensures that the footers and taglines end up on the right page.
	return s


ly_paper_to_latexpaper =  {
	'a4' : 'a4paper',
	'letter' : 'letterpaper', 
}

def global_latex_definition (tfiles, extra):

	'''construct preamble from EXTRA, dump Latex stuff for each
lily output file in TFILES after that, and return the Latex file constructed.  '''


	s = ""
	s = s + '% generation tag\n'

	options = ''

	if extra['papersize']:
		try:
			options = '%s' % ly_paper_to_latexpaper[extra['papersize'][0]]
		except KeyError:
			warning (_ ("invalid value: %s") % `extra['papersize'][0]`)
			pass

	if extra['latexoptions']:
		options = options + ',' + extra['latexoptions'][-1]

	s = s + '\\documentclass[%s]{article}\n' % options

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
	maxlw = max (extra['linewidth'] + [-1])
	if maxlw < 0:
	        # who the hell is 597 ?
		linewidth = '597'
	else:
		linewidth = maxlw
	s = s + '\geometry{width=%spt%s,headheight=2mm,footskip=2mm,%s}\n' % (linewidth, textheight, orientation)

	if extra['latexoptions']:
		s = s + '\geometry{twosideshift=4mm}\n'

	s = s + r'''
\usepackage[latin1]{inputenc}
\input{titledefs}
'''
	
	if extra['pagenumber'] and extra['pagenumber'][-1] and extra['pagenumber'][-1] != 'no':
		s = s + '\setcounter{page}{%s}\n' % (extra['pagenumber'][-1])
                s = s + '\\pagestyle{plain}\n'
	else:
		s = s + '\\pagestyle{empty}\n'

	s = s + '\\begin{document}\n'
	s = s + '\\thispagestyle{firstpage}\n'

	first = 1
	for t in tfiles:
		s = s + one_latex_definition (t, first)
		first = 0


	s = s + '\\thispagestyle{lastpage}\n'
	s = s + '\\end{document}'

	return s

def run_latex (files, outbase, extra):

	"""Construct latex file, for FILES and EXTRA, dump it into
OUTBASE.latex. Run LaTeX on it.

RETURN VALUE

None
	"""
	latex_fn = outbase + '.latex'
	
	wfs = find_tex_files (files, extra)
	s = global_latex_definition (wfs, extra)

	f = open (latex_fn, 'w')
	f.write (s)
	f.close ()

	cmd = 'latex \\\\nonstopmode \\\\input %s' % latex_fn

	if not verbose_p:
		progress ( _("Running %s...") % 'LaTeX')
		cmd = cmd + ' 1> /dev/null 2> /dev/null'

	system (cmd)

def run_dvips (outbase, extra):


	"""Run dvips using the correct options taken from EXTRA,
leaving a PS file in OUTBASE.ps

RETURN VALUE

None.
"""
	opts = ''
	if extra['papersize']:
		opts = opts + ' -t%s' % extra['papersize'][0]

	if extra['orientation'] and extra['orientation'][0] == 'landscape':
		opts = opts + ' -tlandscape'

	cmd = 'dvips %s -o%s %s' % (opts, outbase + '.ps', outbase + '.dvi')
	
	if not verbose_p:
		progress ( _("Running %s...") % 'dvips')
		cmd = cmd + ' 1> /dev/null 2> /dev/null'
		
	system (cmd)

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

def find_file_in_path (path, name):
	for d in string.split (path, os.pathsep):
		if name in os.listdir (d):
			return os.path.join (d, name)

# Added as functionality to ly2dvi, because ly2dvi may well need to do this
# in future too.
PS = '%!PS-Adobe'
def find_pfa_fonts (name):
	s = open (name).read ()
	if s[:len (PS)] != PS:
		# no ps header?
		errorport.write (_( "error: ") + _ ("not a PostScript file: `%s\'" % name))
		errorport.write ('\n')
		sys.exit (1)
	here = 0
	m = re.match ('.*?/(feta[-a-z0-9]+) +findfont', s[here:], re.DOTALL)
	pfa = []
	while m:
		here = m.end (1)
		pfa.append (m.group (1))
		m = re.match ('.*?/(feta[-a-z0-9]+) +findfont', s[here:], re.DOTALL)
	return pfa

	
(sh, long) = getopt_args (option_definitions)
try:
	(options, files) = getopt.getopt(sys.argv[1:], sh, long)
except getopt.error, s:
	errorport.write ('\n')
	errorport.write (_ ("error: ") + _ ("getopt says: `%s\'" % s))
	errorport.write ('\n')
	errorport.write ('\n')
	help ()
	sys.exit (2)
	
for opt in options:	
	o = opt[0]
	a = opt[1]

	if 0:
		pass
	elif o == '--help' or o == '-h':
		help ()
		sys.exit (0)
	elif o == '--find-pfa' or o == '-f':
		fonts = map (lambda x: x + '.pfa', find_pfa_fonts (a))
		files = map (lambda x:
			     find_file_in_path (os.environ['GS_FONTPATH'], x),
			     fonts)
		print string.join (files, ' ')
		sys.exit (0)
	elif o == '--include' or o == '-I':
		include_path.append (a)
	elif o == '--postscript' or o == '-P':
		targets['PS'] = 0
	elif o == '--keep' or o == '-k':
		keep_temp_dir_p = 1
	elif o == '--no-lily':
		lily_p = 0
	elif o == '--no-paper' or o == '-m':
		targets = {}
		targets['MIDI'] = 0
		paper_p = 0
	elif o == '--output' or o == '-o':
		output_name = a
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
		status = system ('lilypond -w', ignore_error = 1)
		if status:
			warranty ()

		sys.exit (0)


def cp_to_dir (pattern, dir):
	"Copy files matching re PATTERN from cwd to DIR"
	# Duh.  Python style portable: cp *.EXT OUTDIR
	# system ('cp *.%s %s' % (ext, outdir), 1)
	files = filter (lambda x, p=pattern: re.match (p, x), os.listdir ('.'))
	map (lambda x, d=dir: shutil.copy2 (x, os.path.join (d, x)), files)

# Python < 1.5.2 compatibility
#
# On most platforms, this is equivalent to
#`normpath(join(os.getcwd()), PATH)'.  *Added in Python version 1.5.2*
if os.path.__dict__.has_key ('abspath'):
	abspath = os.path.abspath
else:
	def abspath (path):
		return os.path.normpath (os.path.join (os.getcwd (), path))

if os.__dict__.has_key ('makedirs'):
	makedirs = os.makedirs
else:
	def makedirs (dir, mode=0777):
		system ('mkdir -p %s' % dir)

def mkdir_p (dir, mode=0777):
	if not os.path.isdir (dir):
		makedirs (dir, mode)

include_path = map (abspath, include_path)

original_output = output_name


if files and files[0] != '-':
	
	# Ugh, maybe make a setup () function
	files = map (lambda x: strip_extension (x, '.ly'), files)

	# hmmm. Wish I'd 've written comments when I wrote this.
	# now it looks complicated.
	
	(outdir, outbase) = ('','')
	if not output_name:
		outbase = os.path.basename (files[0])
		outdir = abspath('.')
	elif output_name[-1] == os.sep:
		outdir = abspath (output_name)
		outbase = os.path.basename (files[0])
	else:
		(outdir, outbase) = os.path.split (abspath (output_name))

	for i in ('.dvi', '.latex', '.ly', '.ps', '.tex'):
		output_name = strip_extension (output_name, i)
		outbase = strip_extension (outbase, i)
	files = map (abspath, files)

	for i in files[:] + [output_name]:
		if string.find (i, ' ') >= 0:
			user_error (_ ("filename should not contain spaces: `%s'") % i)
			
	if os.path.dirname (output_name) != '.':
		dep_prefix = os.path.dirname (output_name)
	else:
		dep_prefix = 0

	reldir = os.path.dirname (output_name)
	if outdir != '.' and (track_dependencies_p or targets.keys ()):
		mkdir_p (outdir, 0777)

	setup_environment ()
	tmpdir = setup_temp ()
	if cache_pks_p :
		os.chdir (outdir)
		cp_to_dir (PK_PATTERN, tmpdir)

	# to be sure, add tmpdir *in front* of inclusion path.
	#os.environ['TEXINPUTS'] =  tmpdir + ':' + os.environ['TEXINPUTS']
	os.chdir (tmpdir)
	
	if lily_p:
		try:
			run_lilypond (files, outbase, dep_prefix)
		except:
 			# TODO: friendly message about LilyPond setup/failing?
 			#
 			# TODO: lilypond should fail with different
 			# error codes for:
 			#   - guile setup/startup failure
 			#   - font setup failure
 			#   - init.ly setup failure
 			#   - parse error in .ly
 			#   - unexpected: assert/core dump
			targets = {}
			traceback.print_exc ()

	if targets.has_key ('DVI') or targets.has_key ('PS'):
		try:
			run_latex (files, outbase, extra_init)
			# unless: add --tex, or --latex?
			del targets['TEX']
			del targets['LATEX']
		except:
			# TODO: friendly message about TeX/LaTeX setup,
			# trying to run tex/latex by hand
			if targets.has_key ('DVI'):
				del targets['DVI']
			if targets.has_key ('PS'):
				del targets['PS']
			traceback.print_exc ()

	if targets.has_key ('PS'):
		try:
			run_dvips (outbase, extra_init)
		except: 
			if targets.has_key ('PS'):
				del targets['PS']
			traceback.print_exc ()

	# add DEP to targets?
	if track_dependencies_p:
		depfile = os.path.join (outdir, outbase + '.dep')
		generate_dependency_file (depfile, depfile)
		if os.path.isfile (depfile):
			progress (_ ("dependencies output to `%s'...") % depfile)

	# Hmm, if this were a function, we could call it the except: clauses
	for i in targets.keys ():
		ext = string.lower (i)
		cp_to_dir ('.*\.%s$' % ext, outdir)
		outname = outbase + '.' + string.lower (i)
		abs = os.path.join (outdir, outname)
		if reldir != '.':
			outname = os.path.join (reldir, outname)
		if os.path.isfile (abs):
			progress (_ ("%s output to `%s'...") % (i, outname))
		elif verbose_p:
			warning (_ ("can't find file: `%s'") % outname)
			
		if cache_pks_p:
			cp_to_dir (PK_PATTERN, outdir)
		
	os.chdir (original_dir)
	cleanup_temp ()
	
else:
	# FIXME: read from stdin when files[0] = '-'
	help ()
	user_error (_ ("no files specified on command line."), 2)



