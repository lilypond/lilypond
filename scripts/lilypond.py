#!@PYTHON@
#
# lilypond.py -- Run LilyPond, add titles to bare score, generate printable
#              document
#              Invokes: lilypond-bin, latex (or pdflatex), dvips, ps2pdf, gs
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

# This is the third incarnation of ly2dvi, now renamed lilypond.
#
# Earlier incarnations of lilypond were written by
# Jeffrey B. Reed<daboys@austin.rr.com> (Python version)
# Jan Arne Fagertun <Jan.A.Fagertun@@energy.sintef.no> (Bourne shell script)
#

# Note: gettext work best if we use ' for program/docstrings and "
#       for gettextable strings.
#       USE ''' for docstrings.


'''
TODO:

  * figure out which set of command line options should make lilypond:

      na: create tex only?  
      na: create latex only? 
      na: create tex and latex
      default: create dvi only
      na: create tex, latex and dvi
      -P: create dvi and ps
      -p: create pdf
      na: * create ps only

     etc.

  * move versatile taglines, 
  
     \header {
        beginfooter=\mutopiaPD
        endfooter=\tagline  -> 'lily was here <version>'
     }

     lilytagline (->lily was here), usertagline, copyright, lily-version
     etc.

  * head/header tagline/endfooter

  * dvi from lilypond .tex output?  This is hairy, because we create dvi
    from lilypond .tex *and* header output.

  * multiple \score blocks?

  * Introduce verbosity levels
  
     0  = QUIET: mute all command output, no lilypond progress
     1  = BRIEF: mute all command output, only lilypond progress
     2a = NORMAL: show only LilyPond command output, show lilypond progress
     2b = NORMAL: show command output, show lilypond progress
     3  = VERBOSE: show command output, run lilypond --verbose
     4  = DEBUGGING: show all command output, run lilypond --verbose, print
                   environment and all kinds of client side debugging stuff

     Currently, we only have 1 and 4, but we kludge to have 2a and 4.
'''

import operator
import stat
import string
import traceback
import glob

################################################################
# Users of python modules should include this snippet
# and customize variables below.

# We'll suffer this path init stuff as long as we don't install our
# python packages in <prefix>/lib/pythonx.y (and don't kludge around
# it as we do with teTeX on Red Hat Linux: set some environment var
# (PYTHONPATH) in profile)

# If set, LILYPONDPREFIX must take prevalence
# if datadir is not set, we're doing a build and LILYPONDPREFIX
import getopt, os, sys
datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
	datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPONDPREFIX') :
	datadir = os.environ['LILYPONDPREFIX']
	while datadir[-1] == os.sep:
		datadir= datadir[:-1]


sys.path.insert (0, os.path.join (datadir, 'python'))

# Customize these
#if __name__ == '__main__':

import lilylib as ly
global _;_=ly._
global re;re = ly.re

# lilylib globals
program_name = 'lilypond'
program_version = '@TOPLEVEL_VERSION@'
verbose_p = 0
pseudo_filter_p = 0
original_dir = os.getcwd ()
temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)
keep_temp_dir_p = 0
preview_resolution = 90
debug_p = 0

## FIXME
## do -P or -p by default?
##help_summary = _ ("Run LilyPond using LaTeX for titling")
help_summary = _ ("Run LilyPond, add titles, generate printable document.")
copyright = ('Han-Wen Nienhuys <hanwen@cs.uu.nl',
	     'Jan Nieuwenhuizen <janneke@gnu.org')

option_definitions = [
	('', 'd', 'dependencies',
	 _ ("write Makefile dependencies for every input file")),
	('', 'h', 'help', _ ("print this help")),
	('', '', 'debug', _ ("print even more output")),
	(_ ("FILE"), 'f', 'find-pfa', _ ("find pfa fonts used in FILE")),
	('','', 'html', _("make HTML file with links to all output")),
	(_ ("DIR"), 'I', 'include', _ ("add DIR to LilyPond's search path")),
	('', 'k', 'keep',
	 _ ("keep all output, output to directory %s.dir") % program_name),
	('', '', 'no-lily', _ ("don't run LilyPond")),
	('', 'm', 'no-paper', _ ("produce MIDI output only")),
	(_ ("FILE"), 'o', 'output', _ ("write output to FILE")),
	(_ ('RES'), '', 'preview-resolution',
	 _ ("set the resolution of the preview to RES")),
	('', '', 'no-pdf', _ ("do not generate PDF output")),
	('', '', 'no-ps', _ ("do not generate PostScript output")),
	('', 'p', 'pdf', _ ("generate PDF output")),
	('', 'P', 'postscript', _ ("generate PostScript output")),
	('', '', 'pdftex', _ ("use pdflatex to generate PDF output")),
	('', '', 'png', _("generate PNG page images")),
	('', '', 'preview', _ ("make a picture of the first system")),
	('', '', 'psgz', _ ("generate PS.GZ")),
	('', 's', 'safe-mode', _ ("run in safe-mode")),
	(_ ("KEY=VAL"), 'S', 'set', _ ("change global setting KEY to VAL")),
	('', 'V', 'verbose', _ ("be verbose")),
	('', 'v', 'version', _ ("print version number")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

# other globals
safe_mode_p = 0
preview_p = 0
page_images_p = 0
lilypond_error_p = 0
html_p = 0

# Pdftex support
pdftex_p = 0
latex_cmd = 'latex'


tex_extension = '.tex'  ## yuk.

#lilypond_binary = 'valgrind --suppressions=%(home)s/usr/src/guile-1.6.supp --num-callers=10 %(home)s/usr/src/lilypond/lily/out/lilypond '% { 'home' : '/home/hanwen' }

lilypond_binary = os.path.join ('@bindir@', 'lilypond-bin')

# only use installed binary  when we're installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
	lilypond_binary = 'lilypond-bin'


layout_fields = ['dedication', 'title', 'subtitle', 'subsubtitle',
	  'footer', 'head', 'composer', 'arranger', 'instrument',
	  'opus', 'piece', 'metre', 'meter', 'poet', 'texttranslator']


# init to empty; values here take precedence over values in the file

## TODO: change name.
extra_init = {
	'fontencoding' : [],
	'inputencoding' : ['latin1'],
	'language' : [],
	'latexheaders' : [],
	'latexpackages' :  ['geometry'],

	# for geometry v3
	'latexoptions' : ['compat2'],
	
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

# Output formats that lilypond should create
targets = ['DVI', 'LATEX', 'MIDI', 'TEX', 'PDF', 'PS']

track_dependencies_p = 0
dependency_files = []

#what a name.
def set_setting (dict, key, val):
	try:
		val = string.atoi (val)
	except ValueError:
		#ly.warning (_ ("invalid value: %s") % `val`)
		pass

	if type(val) == type ('hoi'):
		try:
			val = string.atof (val)
		except ValueError:
			#ly.warning (_ ("invalid value: %s") % `val`)
			pass

	try:
		dict[key].append (val)
	except KeyError:
		ly.warning (_ ("no such setting: `%s'") % `key`)
		dict[key] = [val]


def escape_shell (x):
	return re.sub ("(\s|[`'\"\\\\])", r'\\\1',x)


def run_lilypond (files, dep_prefix):
	def make_include_option (x):
		return '-I %s' %   escape_shell (x)

	opts = ''
	opts = opts + ' ' + string.join (map (make_include_option, include_path))
	if pseudo_filter_p:
		opts = opts + ' --output=lelie'
	if paper_p:
		opts = opts + ' ' + string.join (map (lambda x : '-H ' + x,
						      fields))
	else:
		opts = opts + ' --no-paper'

	if pdftex_p:
		opts = opts + ' -f pdftex'		

	if safe_mode_p:
		opts = opts + ' --safe-mode'

	if track_dependencies_p:
		opts = opts + " --dependencies"
		if dep_prefix:
			opts = opts + ' --dep-prefix=%s' % dep_prefix

	fs = string.join (map (escape_shell, files))

	global verbose_p
	if verbose_p:
		opts = opts + ' --verbose'

	if debug_p:
		ly.print_environment ()
		
	cmd = string.join ((lilypond_binary, opts, fs))
	status = ly.system (cmd, ignore_error = 1, progress_p = 1)
	signal = 0x0f & status
	exit_status = status >> 8

	# 2 == user interrupt.
	if signal and signal != 2:
		sys.stderr.write ('\n\n')
		ly.error (_ ("LilyPond crashed (signal %d).") % signal)
		ly.error (_ ("Please submit a bug report to bug-lilypond@gnu.org"))
		ly.exit (status)
			
	if status:
		global lilypond_error_p
		sys.stderr.write ('\n')
		if len (files) == 1:
			ly.error (_ ("LilyPond failed on input file %s (exit status %d)") % (files[0], exit_status))
			lilypond_error_p = 1
			ly.exit (status)
		else:
			ly.error (_ ("LilyPond failed on an input file (exit status %d)") % exit_status)
			ly.error (_ ("Continuing..."))
			lilypond_error_p = 1
		

def analyse_lilypond_output (filename, extra):
	
	# urg
	'''Grep FILENAME for interesting stuff, and
	put relevant info into EXTRA.'''
	filename = filename + tex_extension
	ly.progress (_ ("Analyzing %s...") % filename)
	s = open (filename).read ()

	# search only the first 10k
	s = s[:10240]
	for x in extra_fields:
		m = re.search (r'\\def\\lilypondpaper%s{([^}]*)}'%x, s)
		if m:
			set_setting (extra, x, m.group (1))
	ly.progress ('\n')

def find_tex_files_for_base (base, extra):
	'''
	Find the \header fields dumped from BASE.
	'''
	
	headerfiles = {}
	for f in layout_fields:
		fn = base + '.' + f
		if os.path.exists (fn):
			headerfiles[f] = fn

	if os.path.exists (base  +'.dep'):
		dependency_files.append (base + '.dep')

	for f in extra_fields:
		fn =base + '.' + f
		if os.path.exists (fn):
			extra[f].append (open (fn).read ())
	
	return (base + tex_extension, headerfiles)
	 

def find_tex_files (files, extra):
	'''
	Find all .tex files whose prefixes start with some name in FILES. 

	'''
	
	tfiles = []
	
	for f in files:
		x = 0
		while 1:
			fname = os.path.basename (f)
			fname = ly.strip_extension (fname, '.ly')
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
		ly.warning (_ ("no LilyPond output found for `%s'") % fstr)
	return tfiles

def one_latex_definition (defn, first):
	s = '\n'
	for (k,v) in defn[1].items ():
		val = open (v).read ()
		if (string.strip (val)):
			s += r'''\def\lilypond%s{%s}''' % (k, val)
		else:
			s += r'''\let\lilypond%s\relax''' % k
		s += '\n'

	if first:
		s += '\\def\\mustmakelilypondtitle{}\n'
	else:
		s += '\\def\\mustmakelilypondpiecetitle{}\n'
		
	s += '\\input %s\n' % defn[0] # The final \n seems important here. It ensures that the footers and taglines end up on the right page.
	return s


ly_paper_to_latexpaper = {
	'letter' : 'letterpaper', 
	'a3' : 'a3paper',
	'a4' : 'a4paper',
	'a5' : 'a5paper',
	'a6' : 'a6paper',
	'legal' : 'legalpaper', 
	'tabloid' : 'papersize={11in,17in}', 
}

#TODO: should set textheight (enlarge) depending on papersize. 
def global_latex_preamble (extra):
	'''construct preamble from EXTRA,'''
	s = ""
	s += '% generation tag\n'

	options = ''

	if extra['latexoptions']:
		options = options + ',' + extra['latexoptions'][-1]

	s += '\\documentclass[%s]{article}\n' % options

	if safe_mode_p:
		s += '\\nofiles\n'

	s += '\\usepackage{%s}\n' % string.join (extra['latexpackages'], ',')

	if extra['language']:
		s += r'\usepackage[%s]{babel}' % extra['language'][-1] + '\n'

	if extra['fontencoding']:
		s += '\\usepackage[%s]{fontenc}\n' % extra['fontencoding'][-1]

	if extra['inputencoding']:
		s += '\\usepackage[%s]{inputenc}\n' % extra['inputencoding'][-1]

	if extra['latexheaders']:
		s += '\\include{%s}\n' \
			% string.join (extra['latexheaders'], '}\n\\include{')

 	unit = extra['unit'][-1]

	papersize = ''
	if extra['papersize']:
		try:
			papersize = ly_paper_to_latexpaper[extra['papersize'][0]] + ','
		except KeyError:
			ly.warning (_ ("invalid value: `%s'") % `extra['papersize'][0]`)
			pass

	# Default setting: textheight is determined implicitly:
	textheight = ',bottom=11mm,top=12mm'
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
	s += '\geometry{%swidth=%s%s,headsep=2mm,headheight=2mm,footskip=5mm,%s}\n' % (papersize, linewidth, textheight, orientation)


	if 'twoside' in  extra['latexoptions'] :
		s += '\geometry{twosideshift=4mm}\n'

	s += r'''
\input{titledefs}
'''
	
	if extra['pagenumber'] and extra['pagenumber'][-1] and extra['pagenumber'][-1] != 'no':
		s += '\setcounter{page}{%d}\n' % (extra['pagenumber'][-1])
                s += '\\pagestyle{plain}\n'
	else:
		s += '\\pagestyle{empty}\n'


	return s

	
def global_latex_definition (tfiles, extra):
	'''construct preamble from EXTRA, dump Latex stuff for each
lily output file in TFILES after that, and return the Latex file constructed.  '''

	
	s = global_latex_preamble (extra) + '\\begin{document}\n'
	s += '\\parindent 0pt\n'
	s += '\\thispagestyle{firstpage}\n'

	first = 1
	for t in tfiles:
		s += one_latex_definition (t, first)
		first = 0


	s += '\n\\thispagestyle{lastpage}\n'
	s += '\\end{document}'

	return s

def run_latex (files, outbase, extra):

	'''Construct latex file, for FILES and EXTRA, dump it into
OUTBASE.latex. Run LaTeX on it.

RETURN VALUE

None
	'''

	latex_fn = outbase + '.latex'
	
	wfs = find_tex_files (files, extra)
	s = global_latex_definition (wfs, extra)

	f = open (latex_fn, 'w')
	f.write (s)
	f.close ()

	cmd = latex_cmd + ' \\\\nonstopmode \\\\input %s' % latex_fn
	
	# Ugh.  (La)TeX writes progress and error messages on stdout
	# Redirect to stderr
	cmd = '(( %s  >&2 ) >&- )' % cmd
	status = ly.system (cmd, ignore_error = 1)
	signal = 0xf & status
 	exit_status = status >> 8

	if exit_status:

		logstr = ''
		try:
			logstr = open (outbase + '.log').read ()
			m = re.search ("\n!", logstr)
			start = m.start (0)
			logstr = logstr[start:start+200]
		except:
			pass
			
		ly.error (_ ("LaTeX failed on the output file."))
		ly.error (_ ("The error log is as follows:"))
		sys.stderr.write (logstr + '\n')
		ly.exit (1)
	
	if preview_p:
		# make a preview by rendering only the 1st line
		# of each score
		for score in find_tex_files (files, extra):
			preview_base = ly.strip_extension (score[0], '.tex')
			preview_fn = preview_base + '.preview.tex'
			s = global_latex_definition ((score,), extra)
			s = re.sub ('thispagestyle{firstpage}',
				    r'''thispagestyle{empty}%
				    \\def\\interscoreline{\\endinput}''', s)
			s = re.sub ('thispagestyle{lastpage}',
				    r'''thispagestyle{empty}%
				    \\def\\interscoreline{\\endinput}''', s)
			f = open (preview_fn, 'w')
			f.write (s)
			f.close ()
			cmd = '%s \\\\nonstopmode \\\\input %s' \
			      % (latex_cmd, preview_fn)
			ly.system (cmd)


def run_dvips (outbase, extra):


	'''Run dvips using the correct options taken from EXTRA,
leaving a PS file in OUTBASE.ps

RETURN VALUE

None.
'''
	opts = ''
	if extra['papersize']:
		opts = opts + ' -t%s' % extra['papersize'][0]

	if extra['orientation'] and extra['orientation'][0] == 'landscape':
		opts = opts + ' -tlandscape'


	if 'PDF' in targets:
		where = ly.read_pipe ('kpsewhich feta20.pfa').strip()

		pfa_file  = None
		if where:
			try: 
				pfa_file = open (where, 'r')
			except IOError:
				pass

		if pfa_file:
			opts = opts + ' -Ppdf -G0 -u +lilypond.map'
		else:
			ly.warning (_ ('''Trying create PDF, but no PFA fonts found.
Using bitmap fonts instead. This will look bad.'''))

	cmd = 'dvips %s -o%s %s' % (opts, outbase + '.ps', outbase + '.dvi')
	ly.system (cmd)

	if preview_p:
		for score in find_tex_files (files, extra):
			preview_base = ly.strip_extension (score[0], '.tex')
			cmd = 'dvips -E -Ppdf -u+lilypond.map -o%s %s' \
			      % (preview_base + '.preview.ps',
				 preview_base + '.preview.dvi')
			ly.system (cmd)

	if 'PDF' in targets:
		cmd = 'ps2pdf %s.ps %s.pdf' % (outbase , outbase)
		ly.system (cmd)
		
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
		if not d:
			d = original_dir
		if name in os.listdir (d):
			return os.path.join (d, name)

# Added as functionality to lilypond, because lilypond may well need to do this
# in future too.
PS = '%!PS-Adobe'
def find_pfa_fonts (name):
	s = open (name).read ()
	if s[:len (PS)] != PS:
		# no ps header?
		ly.error (_ ("not a PostScript file: `%s\'" % name))
		ly.exit (1)
	here = 0
	m = re.match ('.*?/([-a-zA-Z]*(feta|parmesan)[-a-z0-9]+) +findfont', s[here:], re.DOTALL)
	pfa = []
	while m:
		here = here + m.end (0)
		pfa.append (m.group (1))
		m = re.match ('.*?/([-a-zA-Z]*(feta|parmesan)[-a-z0-9]+) +findfont', s[here:], re.DOTALL)
	return pfa


def make_html_menu_file (html_file, files_found):
	exts = {
		'pdf' : "Print (PDF, %s)",
		'ps.gz' : "Print (gzipped PostScript, %s)",
		'png' : "View (PNG, %s)",
		'midi' : "Listen (MIDI, %s)",
		'ly' : "View source code (%s)", 
		}
	html_str = ''

	pages = filter (lambda x : re.search ('page[0-9]+.png',  x),
			files_found)
	rest = filter (lambda x : not re.search ('page[0-9]+.png',  x),
			files_found)

	preview = filter (lambda x: re.search ('.png$', x), rest)
	if preview:
		html_str = '<img src="%s">' % preview[0]

	for p in pages:
		page = re.sub ('.*page([0-9])+.*', 'View page \\1 (PNG picture, %s)\n', p)
		page = page % 'unknown size'
		
		html_str += '<li><a href="%s">%s</a>' % (p, page)
		
		
	for e in ['pdf', 'ps.gz', 'midi', 'ly']:
		fs = filter (lambda x: re.search ('.%s$' % e, x), rest)
		for f in fs:
			entry = exts[e] % 'unknown size' # todo
			html_str += '<li><a href="%s">%s</a>\n\n' % (f, entry)

	html_str += "\n\n</li>"
	ly.progress (_("Writing HTML menu `%s'") % html_file)
 	ly.progress ('\n')
	open (html_file, 'w').write (html_str)
	
################################################################
## MAIN
################################################################

(sh, long) = ly.getopt_args (option_definitions)
try:
	(options, files) = getopt.getopt (sys.argv[1:], sh, long)
except getopt.error, s:
	sys.stderr.write ('\n')
	ly.error (_ ("getopt says: `%s\'" % s))
	sys.stderr.write ('\n')
	ly.help ()
	ly.exit (2)
	
for opt in options:
	o = opt[0]
	a = opt[1]

	if 0:
		pass
	elif o == '--help' or o == '-h':
		ly.help ()
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
		if 'PDF' in targets:
			targets.remove ('PDF')
		if 'PS' not in targets:
			targets.append ('PS')
	elif o == '--pdf' or o == '-p':
		if 'PDF' not in targets:
			targets.append ('PDF')
	elif o == '--no-pdf':
		if 'PDF' in targets:
			targets.remove ('PDF')
	elif o == '--no-ps':
		if 'PS' in targets:
			targets.remove ('PS')
		if 'PDF' in targets:
			targets.remove ('PDF')
	elif o == '--keep' or o == '-k':
		keep_temp_dir_p = 1
	elif o == '--debug':
		verbose_p = 1
		debug_p = 1 
	elif o == '--no-lily':
		lily_p = 0
	elif o == '--preview':
		preview_p = 1
		if 'PNG' not in targets:
			targets.append ('PNG')
	elif o == '--preview-resolution':
		preview_resolution = string.atoi (a)
	elif o == '--no-paper' or o == '-m':
		targets = ['MIDI'] 
		paper_p = 0
	elif o == '--output' or o == '-o':
		output_name = a
	elif o == '--safe-mode' or o == '-s':
		safe_mode_p = 1
	elif o == '--set' or o == '-S':
		ss = string.split (a, '=')
		set_setting (extra_init, ss[0], ss[1])
	elif o == '--dependencies' or o == '-d':
		track_dependencies_p = 1
	elif o == '--verbose' or o == '-V':
		verbose_p = 1
	elif o == '--version' or o == '-v':
		ly.identify (sys.stdout)
		sys.exit (0)
	elif o == '--pdftex':
		latex_cmd = 'pdflatex'
		targets.remove ('DVI')
		targets.append ('PDFTEX')
		pdftex_p = 1
		tex_extension = '.pdftex'
	elif o == '--warranty' or o == '-w':
		status = os.system ('%s -w' % lilypond_binary)
		if status:
			ly.warranty ()
		sys.exit (0)
	elif o == '--html':
		html_p = 1
	elif o == '--png':
		page_images_p = 1
		if 'PNG' not in targets:
			targets.append ('PNG')
	else:
		unimplemented_option () # signal programming error

# Don't convert input files to abspath, rather prepend '.' to include
# path.
include_path.insert (0, '.')

# As a neat trick, add directory part of first input file
# to include path.  That way you can do without the clumsy -I in:

#    lilypond -I foe/bar/baz foo/bar/baz/baz.ly
if files and files[0] != '-' and os.path.dirname (files[0]) != '.':
	include_path.append (os.path.dirname (files[0]))
	
include_path = map (ly.abspath, include_path)

if files and (files[0] == '-' or output_name == '-'):
	if len (files) == 1:
		pseudo_filter_p = 1
		output_name = 'lelie'
		if verbose_p:
			ly.progress (_ ("pseudo filter") + '\n')
	else:
		ly.help ()
		ly.error (_ ("pseudo filter only for single input file"))
		ly.exit (2)
		
if not files:
	ly.help ()
	ly.error (_ ("no files specified on command line"))
	ly.exit (2)

if 1:
	ly.identify (sys.stderr)
	ly.lilypond_version_check (lilypond_binary, '@TOPLEVEL_VERSION@')
	
	original_output = output_name
	
	# Ugh, maybe make a setup () function
	files = map (lambda x: ly.strip_extension (x, '.ly'), files)

	# hmmm. Wish I'd 've written comments when I wrote this.
	# now it looks complicated.
	
	(outdir, outbase) = ('','')
	if not output_name:
		outbase = os.path.basename (files[0])
		outdir = ly.abspath ('.')
	elif output_name[-1] == os.sep:
		outdir = ly.abspath (output_name)
		outbase = os.path.basename (files[0])
	else:
		(outdir, outbase) = os.path.split (ly.abspath (output_name))

	for i in ('.dvi', '.latex', '.ly', '.ps', '.tex', '.pdftex'):
		output_name = ly.strip_extension (output_name, i)
		outbase = ly.strip_extension (outbase, i)

	for i in files[:] + [output_name]:
		b = os.path.basename (i)
		if string.find (b, ' ') >= 0:
			ly.error (_ ("filename should not contain spaces: `%s'") % b)
			ly.exit (1)
			
	if os.path.dirname (output_name) != '.':
		dep_prefix = os.path.dirname (output_name)
	else:
		dep_prefix = 0

	reldir = os.path.dirname (output_name)
	if outdir != '.' and (track_dependencies_p or targets):
		ly.mkdir_p (outdir, 0777)

	tmpdir = ly.setup_temp ()
	ly.setup_environment ()
	if safe_mode_p:
		os.environ['openout_any'] = 'p'

	# to be sure, add tmpdir *in front* of inclusion path.
	#os.environ['TEXINPUTS'] = tmpdir + ':' + os.environ['TEXINPUTS']
	os.chdir (tmpdir)

	# We catch all exceptions, because we need to do stuff at exit:
	#   * copy any successfully generated stuff from tempdir and
	#     notify user of that
	#   * cleanout tempdir
	if lily_p:
		try:
			run_lilypond (files, dep_prefix)
		except:
			### ARGH. This also catches python programming errors.
			### this should only catch lilypond nonzero exit  status
			### --hwn

			
 			# TODO: friendly message about LilyPond setup/failing?
 			#
			targets = []
			if verbose_p:
				traceback.print_exc ()
			else:
				ly.warning (_("Running LilyPond failed. Rerun with --verbose for a trace."))
				
	# Our LilyPond pseudo filter always outputs to 'lelie'
	# have subsequent stages and use 'lelie' output.
	if pseudo_filter_p:
		files[0] = 'lelie'

	if 'PS.GZ'  in targets:
		targets.append ('PS')
		
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
			if verbose_p:
				traceback.print_exc ()

	if 'PS' in targets:
		try:
			run_dvips (outbase, extra_init)
			
		except: 
			if 'PS' in targets:
				targets.remove ('PS')
			if verbose_p:
				traceback.print_exc ()
			else:
				ly.warning (_("Failed to make PS file. Rerun with --verbose for a trace."))

	if preview_p:
		for score in find_tex_files (files, extra_init):
			preview_base = ly.strip_extension (score[0], '.tex')
			ly.make_ps_images (preview_base + '.preview.ps',
					   resolution=preview_resolution
					   )

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
			if verbose_p:
				traceback.print_exc ()
			else:
				ly.warning (_("Running LaTeX failed. Rerun with --verbose for a trace."))
				
	if page_images_p:
		ly.make_ps_images (outbase + '.ps' ,
				   resolution = preview_resolution
				   )

	# add DEP to targets?
	if track_dependencies_p:
		depfile = os.path.join (outdir, outbase + '.dep')
		generate_dependency_file (depfile, depfile)
		if os.path.isfile (depfile):
			ly.progress (_ ("dependencies output to `%s'...") %
				     depfile)
			ly.progress ('\n')

	if pseudo_filter_p:
		main_target = 0
		for i in 'PDF', 'PS', 'PNG', 'DVI', 'LATEX':
			if i in targets:
				main_target = i
				break

		ly.progress (_ ("%s output to <stdout>...") % i)
		outname = outbase + '.' + string.lower (main_target)
		if os.path.isfile (outname):
			sys.stdout.write (open (outname).read ())
		elif verbose_p:
			ly.warning (_ ("can't find file: `%s'") % outname)
		targets = []
		ly.progress ('\n')
		
	if 'PS.GZ' in targets:
		ly.system ("gzip *.ps") 
		targets.remove ('PS')

	# Hmm, if this were a function, we could call it the except: clauses
	files_found = []
	for i in targets:
		ext = string.lower (i)

		pattern = '%s.%s' % (outbase, ext)
		if i == 'PNG':
			pattern  = '*.png' 
		ls = glob.glob (pattern)
		files_found += ls 
		ly.cp_to_dir ('.*\.%s$' % ext, outdir)


		if ls:
			names = string.join (map (lambda x: "`%s'" % x, ls))
			ly.progress (_ ("%s output to %s...") % (i, names))
			ly.progress ('\n')
		elif verbose_p:
			ly.warning (_ ("can't find file: `%s.%s'") % (outbase, ext))

	if html_p:
		make_html_menu_file (os.path.join (outdir, outbase + ".html"),
				     files_found)

	os.chdir (original_dir)
	ly.cleanup_temp ()

	sys.exit (lilypond_error_p)
