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


################################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter

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

# Attempt to fix problems with limited stack size set by Python!
# Sets unlimited stack size. Note that the resource module only
# is available on UNIX.
try:
       import resource
       resource.setrlimit (resource.RLIMIT_STACK, (-1, -1))
except:
       pass

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
	program_version = '1.5.54'

def identify ():
	sys.stdout.write ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
	identify ()
	sys.stdout.write ('\n')
	sys.stdout.write (_ ('Copyright (c) %s by' % ' 2001--2002'))
	sys.stdout.write ('\n')
	sys.stdout.write ('  Han-Wen Nienhuys')
	sys.stdout.write ('  Jan Nieuwenhuizen')
	sys.stdout.write ('\n\n')
	sys.stdout.write ('\n')
	sys.stdout.write (_ ("Distributed under terms of the GNU General Public License.  It comes with NO WARRANTY."))
	sys.stdout.write ('\n')

def progress (s):
	errorport.write (s + '\n')

def warning (s):
	progress (_ ("warning: ") + s)

def user_error (s, e=1):
	errorport.write (program_name + ":" + _ ("error: ") + s + '\n')
	if (e):
		sys.exit (e)
	
def error (s):
	'''Report the error S.

	If verbose is set, exit by raising an exception. Otherwise,
	simply sys.exit().

	Please do not abuse by trying to catch this error. If you do
	not want a stack trace, write to the output directly.

	RETURN VALUE

	None
	
	'''
	
	progress (_ ("error: ") + s)
	if verbose_p:
		raise _ ("Exiting ... ")
	else:
		sys.exit (2)

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


def system (cmd, ignore_error = 0, quiet =0):
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
			if not quiet:
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


# if set, LILYPONDPREFIX must take prevalence
# if datadir is not set, we're doing a build and LILYPONDPREFIX 
datadir = '@local_lilypond_datadir@'

if os.environ.has_key ('LILYPONDPREFIX') :
	datadir = os.environ['LILYPONDPREFIX']
else:
	datadir = '@local_lilypond_datadir@'


while datadir[-1] == os.sep:
	datadir= datadir[:-1]

sys.path.insert (0, os.path.join (datadir, 'python'))

################################################################
# END Library


program_name = 'ly2dvi'

original_dir = os.getcwd ()
temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)
errorport = sys.stderr
keep_temp_dir_p = 0
verbose_p = 0
preview_p = 0
lilypond_error_p = 0
preview_resolution = 90
pseudo_filter_p = 0
latex_cmd = 'latex'
tex_extension = '.tex'
pdftex_p = 0
binary = 'lilypond'
#binary = 'valgrind --suppressions=%(home)s/usr/src/guile-1.6.supp --num-callers=10 %(home)s/usr/src/lilypond/lily/out/lilypond '% { 'home' : '/home/hanwen' }

help_summary = _ ("Run LilyPond using LaTeX for titling")

option_definitions = [
	('', 'd', 'dependencies',
	 _ ("write Makefile dependencies for every input file")),
	('', 'h', 'help', _ ("this help")),
	(_ ("DIR"), 'I', 'include', _ ("add DIR to LilyPond's search path")),
	('', 'k', 'keep',
	 _ ("keep all output, output to directory %s.dir") % program_name),
	('', '', 'no-lily', _ ("don't run LilyPond")),
	('', 'm', 'no-paper', _ ("produce MIDI output only")),
	(_ ("FILE"), 'o', 'output', _ ("write ouput to FILE")),
	(_ ("FILE"), 'f', 'find-pfa', _ ("find pfa fonts used in FILE")),
	(_ ('RES'), '', 'preview-resolution',
	 _ ("set the resolution of the preview to RES")),
	('', 'P', 'postscript', _ ("generate PostScript output")),
	('', 'p', 'pdf', _ ("generate PDF output")),	
	('', '', 'pdftex', _ ("use pdflatex to generate a PDF output")),
	# FIXME: preview, picture; to indicate creation of a PNG?
	('', '', 'preview', _ ("make a picture of the first system")),
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
	'orientation' : [],
	'unit' : ['pt'],
}

extra_fields = extra_init.keys ()
fields = layout_fields + extra_fields

include_path = ['.']
lily_p = 1
paper_p = 1

output_name = ''

# Output formats that ly2dvi should create
targets = ['DVI', 'LATEX', 'MIDI', 'TEX']

track_dependencies_p = 0
dependency_files = []



kpse = os.popen ('kpsexpand \$TEXMF').read()
kpse = re.sub('[ \t\n]+$','', kpse)
type1_paths = os.popen ('kpsewhich -expand-path=\$T1FONTS').read ()

environment = {
	# TODO: * prevent multiple addition.
	#       * clean TEXINPUTS, MFINPUTS, TFMFONTS,
	#         as these take prevalence over $TEXMF
	#         and thus may break tex run?
	'TEXMF' : "{%s,%s}" % (datadir, kpse) ,
	'GS_FONTPATH' : type1_paths,
	'GS_LIB' : datadir + '/ps',
}

# tex needs lots of memory, more than it gets by default on Debian
non_path_environment = {
	'extra_mem_top' : '1000000',
	'extra_mem_bottom' : '1000000',
	'pool_size' : '250000',
}

def setup_environment ():
	# $TEXMF is special, previous value is already taken care of
	if os.environ.has_key ('TEXMF'):
		del os.environ['TEXMF']
 
	for key in environment.keys ():
		val = environment[key]
		if os.environ.has_key (key):
			val = os.environ[key] + os.pathsep + val 
		os.environ[key] = val

	for key in non_path_environment.keys ():
		val = non_path_environment[key]
		os.environ[key] = val

#what a name.
def set_setting (dict, key, val):
	try:
		val = string.atoi (val)
	except ValueError:
		#warning (_ ("invalid value: %s") % `val`)
		pass

	if type(val) == type ('hoi'):
		try:
			val = string.atof (val)
		except ValueError:
			#warning (_ ("invalid value: %s") % `val`)
			pass

	try:
		dict[key].append (val)
	except KeyError:
		warning (_ ("no such setting: `%s'") % `key`)
		dict[key] = [val]


def print_environment ():
	for (k,v) in os.environ.items ():
		sys.stderr.write ("%s=\"%s\"\n" % (k,v)) 

def quiet_system (cmd, name, ignore_error = 0):
	if not verbose_p:
		progress ( _("Running %s...") % name)
		cmd = cmd + ' 1> /dev/null 2> /dev/null'
	elif pseudo_filter_p:
		cmd = cmd + ' 1> /dev/null'

	return system (cmd, ignore_error, quiet = 1)


def run_lilypond (files, dep_prefix):

	opts = ''
	opts = opts + ' ' + string.join (map (lambda x : '-I ' + x,
					      include_path))
	if pseudo_filter_p:
		opts = opts + ' --output=lelie'
	if paper_p:
		opts = opts + ' ' + string.join (map (lambda x : '-H ' + x,
						      fields))
	else:
		opts = opts + ' --no-paper'

	if pdftex_p:
		opts = opts + ' -f pdftex'		

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

	cmd = '%s %s %s ' % (binary, opts, fs)
	if  verbose_p:
		progress ("Invoking `%s'"% cmd)
	status = os.system (cmd)

	signal = 0x0f & status
	exit_status = status >> 8

	# 2 == user interrupt.
	if signal and  signal != 2:
		error ("\n\n" + _ ("LilyPond crashed (signal %d).") % signal \
		       + _ ("Please submit a bug report to bug-lilypond@gnu.org") + "\n")

	if status:
		sys.stderr.write ( "\n" \
			+ _ ("LilyPond failed on an input file (exit status %d).") % exit_status + "\n")
		sys.stderr.write (_("Trying to salvage the rest.") +'\n\n')

		global lilypond_error_p
		lilypond_error_p = 1
		

def analyse_lilypond_output (filename, extra):
	
	# urg
	'''Grep FILENAME for interesting stuff, and
	put relevant info into EXTRA.'''
	filename = filename+tex_extension
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
	
	return (base+tex_extension,headerfiles)
	 

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

			if os.path.exists (fname + tex_extension):
				tfiles.append (find_tex_files_for_base (fname, extra))
				analyse_lilypond_output (fname, extra)
			else:
				break

			x = x + 1
	if not x:
		fstr = string.join (files, ', ')
		warning (_ ("no LilyPond output found for `%s'") % fstr)
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
	'a3' : 'a3paper'
}

#TODO: should set textheight (enlarge) depending on papersize. 
def global_latex_preamble (extra):
	'''construct preamble from EXTRA,'''
	s = ""
	s = s + '% generation tag\n'

	options = ''


	if extra['papersize']:
		try:
			options = ly_paper_to_latexpaper[extra['papersize'][0]]
		except KeyError:
			warning (_ ("invalid value: `%s'") % `extra['papersize'][0]`)
			pass

	if extra['latexoptions']:
		options = options + ',' + extra['latexoptions'][-1]

	s = s + '\\documentclass[%s]{article}\n' % options

	if extra['language']:
		s = s + r'\usepackage[%s]{babel}' % extra['language'][-1] + '\n'


	s = s + '\\usepackage{%s}\n' \
		% string.join (extra['latexpackages'], ',')

	if extra['latexheaders']:
		s = s + '\\include{%s}\n' \
			% string.join (extra['latexheaders'], '}\n\\include{')

 	unit = extra['unit'][-1]

	textheight = ''
	if extra['textheight']:
		textheight = ',textheight=%f%s' % (extra['textheight'][0], unit)

	orientation = 'portrait'
	if extra['orientation']:
		orientation = extra['orientation'][0]

 	# set sane geometry width (a4-width) for linewidth = -1.
	maxlw = max (extra['linewidth'] + [-1])
	if maxlw < 0:
	        # who the hell is 597 ?
		linewidth = '597pt'
	else:
		linewidth = '%d%s' % (maxlw, unit)
	s = s + '\geometry{width=%s%s,headheight=2mm,footskip=2mm,%s}\n' % (linewidth, textheight, orientation)

	if extra['latexoptions']:
		s = s + '\geometry{twosideshift=4mm}\n'

	s = s + r'''
\usepackage[latin1]{inputenc}
\input{titledefs}
'''
	
	if extra['pagenumber'] and extra['pagenumber'][-1] and extra['pagenumber'][-1] != 'no':
		s = s + '\setcounter{page}{%d}\n' % (extra['pagenumber'][-1])
                s = s + '\\pagestyle{plain}\n'
	else:
		s = s + '\\pagestyle{empty}\n'


	return s

	
def global_latex_definition (tfiles, extra):
	'''construct preamble from EXTRA, dump Latex stuff for each
lily output file in TFILES after that, and return the Latex file constructed.  '''

	
	s = global_latex_preamble (extra) + '\\begin{document}\n'
	s = s + '\\parindent 0pt\n'
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

	cmd = latex_cmd + ' \\\\nonstopmode \\\\input %s' % latex_fn
	status = quiet_system (cmd, 'LaTeX', ignore_error = 1)

	signal = 0xf & status
	exit_stat = status >> 8

	if exit_stat:
		logstr = open (outbase + '.log').read()
		m = re.search ("\n!", logstr)
		start = m.start (0)
		logstr = logstr[start:start+200]
		
		user_error (_ ("LaTeX failed on the output file."), 0)
		sys.stderr.write ("\n")
		user_error (_ ("The error log is as follows:"), 0)
		sys.stderr.write ("\n")
		sys.stderr.write (logstr)
		sys.stderr.write ("\n")
		raise 'LaTeX error'
	
	if preview_p:
		# make a preview by rendering only the 1st line.
		preview_fn = outbase + '.preview.tex'
		f = open (preview_fn, 'w')
		f.write (r'''
%s
\input lilyponddefs
\pagestyle{empty}
\begin{document}
\def\interscoreline{\endinput}
\input %s
\end{document}
''' % (global_latex_preamble (extra), outbase))

		f.close()
		cmd = '%s \\\\nonstopmode \\\\input %s' % (latex_cmd, preview_fn)
		quiet_system (cmd, '%s for preview' % latex_cmd)
	

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

	if 'PDF' in targets:
		opts = opts + ' -Ppdf -G0 -u lilypond.map'
		
	cmd = 'dvips %s -o%s %s' % (opts, outbase + '.ps', outbase + '.dvi')
	quiet_system (cmd, 'dvips')

	if preview_p:
		cmd = 'dvips -E -o%s %s' % ( outbase + '.preview.ps', outbase + '.preview.dvi')		
		quiet_system (cmd, 'dvips for preview')

	if 'PDF' in targets:
		cmd = 'ps2pdf %s.ps %s.pdf' % (outbase , outbase)
		quiet_system (cmd, 'ps2pdf')
		
def get_bbox (filename):
	# cut & paste 
	system ('gs -sDEVICE=bbox -q  -sOutputFile=- -dNOPAUSE %s -c quit > %s.bbox 2>&1 ' % (filename, filename))

	box = open (filename + '.bbox').read()
	m = re.match ('^%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)', box)
	gr = []
	if m:
		gr = map (string.atoi, m.groups ())
	
	return gr

#
# cut & paste from lilypond-book.
#
def make_preview (name, extra):
	bbox = get_bbox (name + '.preview.ps')
	margin = 0
	fo = open (name + '.trans.eps' , 'w')
	fo.write ('%d %d translate\n' % (-bbox[0]+margin, -bbox[1]+margin))
	fo.close ()
	
	x = (2* margin + bbox[2] - bbox[0]) * preview_resolution / 72.
	y = (2* margin + bbox[3] - bbox[1]) * preview_resolution / 72.

	cmd = r'''gs -g%dx%d -sDEVICE=pgm  -dTextAlphaBits=4 -dGraphicsAlphaBits=4  -q -sOutputFile=- -r%d -dNOPAUSE %s %s -c quit | pnmtopng > %s'''
	
	cmd = cmd % (x, y, preview_resolution, name + '.trans.eps', name + '.preview.ps',name + '.png')
	quiet_system (cmd, 'gs')

	try:
		status = system (cmd)
	except:
		os.unlink (name + '.png')
		error ("Removing output file")



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
		user_error (_ ("not a PostScript file: `%s\'" % name))
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
	user_error (_ ("getopt says: `%s\'" % s), 0)
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
		targets.append ('PS')
	elif o == '--pdf' or o == '-p':
		targets.append ('PS')
		targets.append ('PDF')
	elif o == '--keep' or o == '-k':
		keep_temp_dir_p = 1
	elif o == '--no-lily':
		lily_p = 0
	elif o == '--preview':
		preview_p = 1
		targets.append ('PNG')
	elif o == '--preview-resolution':
		preview_resolution = string.atoi (a)
	elif o == '--no-paper' or o == '-m':
		targets = ['MIDI'] 
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
	elif o == '--pdftex':
		latex_cmd = 'pdflatex'
		targets.remove('DVI')
		targets.append('PDFTEX')
		pdftex_p = 1
		tex_extension = '.pdftex'
	elif o == '--warranty' or o == '-w':
		status = system ('lilypond -w', ignore_error = 1)
		if status:
			warranty ()

		sys.exit (0)

# Don't convert input files to abspath, rather prepend '.' to include
# path.
include_path.insert (0, '.')

# As a neat trick, add directory part of first input file
# to include path.  That way you can do without the clumsy -I in:

#    ly2dvi -I foe/bar/baz foo/bar/baz/baz.ly
if files and files[0] != '-' and os.path.dirname (files[0]) != '.':
	include_path.append (os.path.dirname (files[0]))
	
include_path = map (abspath, include_path)

if files and (files[0] == '-' or output_name == '-'):
	if len (files) == 1:
		pseudo_filter_p = 1
		output_name = 'lelie'
		if verbose_p:
			progress (_ ("pseudo filter"))
	else:
		help ()
		user_error (_ ("pseudo filter only for single input file"), 2)
		
	
original_output = output_name

if files:
	
	# Ugh, maybe make a setup () function
	files = map (lambda x: strip_extension (x, '.ly'), files)

	# hmmm. Wish I'd 've written comments when I wrote this.
	# now it looks complicated.
	
	(outdir, outbase) = ('','')
	if not output_name:
		outbase = os.path.basename (files[0])
		outdir = abspath ('.')
	elif output_name[-1] == os.sep:
		outdir = abspath (output_name)
		outbase = os.path.basename (files[0])
	else:
		(outdir, outbase) = os.path.split (abspath (output_name))

	for i in ('.dvi', '.latex', '.ly', '.ps', '.tex', '.pdftex'):
		output_name = strip_extension (output_name, i)
		outbase = strip_extension (outbase, i)

	for i in files[:] + [output_name]:
		if string.find (i, ' ') >= 0:
			user_error (_ ("filename should not contain spaces: `%s'") % i)
			
	if os.path.dirname (output_name) != '.':
		dep_prefix = os.path.dirname (output_name)
	else:
		dep_prefix = 0

	reldir = os.path.dirname (output_name)
	if outdir != '.' and (track_dependencies_p or targets):
		mkdir_p (outdir, 0777)

	setup_environment ()
	tmpdir = setup_temp ()

	# to be sure, add tmpdir *in front* of inclusion path.
	#os.environ['TEXINPUTS'] =  tmpdir + ':' + os.environ['TEXINPUTS']
	os.chdir (tmpdir)
	
	if lily_p:
		try:
			run_lilypond (files, dep_prefix)
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
			targets = []
			traceback.print_exc ()

	# Our LilyPond pseudo filter always outputs to 'lelie'
	# have subsequent stages and use 'lelie' output.
	if pseudo_filter_p:
		files[0] = 'lelie'
		
	if 'PNG' in targets and 'PS' not in targets:
		targets.append ('PS')
	if 'PS' in targets and 'DVI' not in targets:
		targets.append('DVI')

	if 'DVI' in targets:
		try:
			run_latex (files, outbase, extra_init)
			# unless: add --tex, or --latex?
			targets.remove ('TEX')
			targets.remove('LATEX')
		except:
			# TODO: friendly message about TeX/LaTeX setup,
			# trying to run tex/latex by hand
			if 'DVI' in targets:
				targets.remove ('DVI')
			if 'PS' in targets:
				targets.remove ('PS')
			traceback.print_exc ()

	if 'PS' in targets:
		try:
			run_dvips (outbase, extra_init)
		except: 
			if 'PS' in targets:
				targets.remove ('PS')
			traceback.print_exc ()

	if 'PNG' in  targets:
		make_preview (outbase, extra_init)

	if 'PDFTEX' in targets:
		try:
			run_latex (files, outbase, extra_init)
			# unless: add --tex, or --latex?
			targets.remove ('TEX')
			targets.remove ('LATEX')
			targets.remove ('PDFTEX')
			if 'PDF' not in targets:
				targets.append('PDF')
		except:
			# TODO: friendly message about TeX/LaTeX setup,
			# trying to run tex/latex by hand
			if 'PDFTEX' in targets:
				targets.remove ('PDFTEX')
			if 'PDF' in targets:
				targets.remove ('PDF')
			if 'PS' in targets:
				targets.remove ('PS')
			traceback.print_exc ()
			sys.exit(1)

	# add DEP to targets?
	if track_dependencies_p:
		depfile = os.path.join (outdir, outbase + '.dep')
		generate_dependency_file (depfile, depfile)
		if os.path.isfile (depfile):
			progress (_ ("dependencies output to `%s'...") %
				  depfile)

	if pseudo_filter_p:
		main_target = 0
		for i in 'PDF', 'PS', 'PNG', 'DVI', 'LATEX':
			if i in targets:
				main_target = i
				break

		outname = outbase + '.' + string.lower (main_target)
		if os.path.isfile (outname):
			sys.stdout.write (open (outname).read ())
		elif verbose_p:
			warning (_ ("can't find file: `%s'") % outname)
		targets = []
		
	# Hmm, if this were a function, we could call it the except: clauses
	for i in targets:
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
			
	os.chdir (original_dir)
	cleanup_temp ()

	sys.exit (lilypond_error_p)
else:
	help ()
	user_error (_ ("no files specified on command line"), 2)
