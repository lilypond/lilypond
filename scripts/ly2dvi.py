#!@PYTHON@
#
# ly2dvi.py -- Run LilyPond, add titles to bare score, generate printable
#              document
#              Invokes: lilypond, latex (or pdflatex), dvips, ps2pdf, gs
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1998--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>

# This is the third incarnation of ly2dvi.
#
# Earlier incarnations of ly2dvi were written by
# Jeffrey B. Reed<daboys@austin.rr.com> (Python version)
# Jan Arne Fagertun <Jan.A.Fagertun@@energy.sintef.no> (Bourne shell script)
#

# Note: gettext work best if we use ' for docstrings and "
#       for gettextable strings.
#       --> DO NOT USE ''' for docstrings.


'''
TODO:

  * figure out which set of command line options should make ly2dvi:

      na: create tex only?  
      na: create latex only? 
      na: create tex and latex
      default: create dvi only
      na: create tex, latex and dvi
      -P: create dvi and ps
      -p: create pdf
      na: * create ps only

     etc.

  * for foo.ly, rename ly2dvi.dir to out-ly2dvi, foo.ly2dvi, foo.dir ?
     
     
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
  
     0  = QUIET: mute all command output, no ly2dvi progress
     1  = BRIEF: mute all command output, only ly2dvi progress
     2a = NORMAL: show only LilyPond command output, show ly2dvi progress
     2b = NORMAL: show command output, show ly2dvi progress
     3  = VERBOSE: show command output, run lilypond --verbose
     4  = DEBUGGING: show all command output, run lilypond --verbose, print
                   environment and all kinds of client side debugging stuff

     Currently, we only have 1 and 4, but we kludge to have 2a and 4.
'''

import operator
import stat
import string
import traceback

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
program_name = 'ly2dvi'
verbose_p = 0
pseudo_filter_p = 0
original_dir = os.getcwd ()
temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)
keep_temp_dir_p = 0
preview_resolution = 90
debug_p = 0

## FIXME
## ly2dvi: silly name?
## do -P or -p by default?
##help_summary = _ ("Run LilyPond using LaTeX for titling")
help_summary = _ ("Run LilyPond, add titles, generate printable document")
copyright = ('Han-Wen Nienhuys <hanwen@cs.uu.nl',
	     'Jan Nieuwenhuizen <janneke@gnu.org')

option_definitions = [
	('', 'd', 'dependencies',
	 _ ("write Makefile dependencies for every input file")),
	('', 'h', 'help', _ ("this help")),
	('', '', 'debug', _ ("print even more output")),
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

# other globals
preview_p = 0
lilypond_error_p = 0

# Pdftex support
pdftex_p = 0
latex_cmd = 'latex'
tex_extension = '.tex'

# Debugging support -- do we need this?
lilypond_cmd = 'lilypond'
#lilypond_cmd = 'valgrind --suppressions=%(home)s/usr/src/guile-1.6.supp --num-callers=10 %(home)s/usr/src/lilypond/lily/out/lilypond '% { 'home' : '/home/hanwen' }


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

	global verbose_p
	if verbose_p:
		opts = opts + ' --verbose'

	if debug_p:
		ly.print_environment ()

	cmd = string.join ((lilypond_cmd,opts, fs))
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
		sys.stderr.write ('\n')
		if len (files) == 1:
			ly.error (_ ("LilyPond failed on input file %s (exit status %d)") % (files[0], exit_status))
			ly.exit (status)
		else:
			ly.error (_ ("LilyPond failed on an input file (exit status %d)") % exit_status)
			ly.error (_ ("Continuing..."))
			global lilypond_error_p
			lilypond_error_p = 1
		

def analyse_lilypond_output (filename, extra):
	
	# urg
	'''Grep FILENAME for interesting stuff, and
	put relevant info into EXTRA.'''
	filename = filename+tex_extension
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
		if os.path.exists (base + '.' + f):
			headerfiles[f] = base+'.'+f

	if os.path.exists (base  +'.dep'):
		dependency_files.append (base + '.dep')

	for f in extra_fields:
		if os.path.exists (base + '.' + f):
			extra[f].append (open (base + '.' + f).read ())
	
	return (base+tex_extension,headerfiles)
	 

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
	'letter' : 'letterpaper', 
	'a3' : 'a3paper',
	'a4' : 'a4paper',
	'a5' : 'a5paper',
	'a6' : 'a6paper',
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
			ly.warning (_ ("invalid value: `%s'") % `extra['papersize'][0]`)
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
	cmd += ' 1>/dev/stderr'
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
		# make a preview by rendering only the 1st line.
		preview_fn = outbase + '.preview.tex'
		f = open (preview_fn, 'w')
		wfs = find_tex_files (files, extra)
		s = global_latex_definition (wfs, extra)

		s = re.sub ('thispagestyle{firstpage}', r'''thispagestyle{empty}%
\\def\\interscoreline{\\endinput}''',s ) 
		s = re.sub ('thispagestyle{lastpage}', r'''thispagestyle{empty}%
\\def\\interscoreline{\\endinput}''',s ) 
		f.write (s)
		f.close()
		cmd = '%s \\\\nonstopmode \\\\input %s' % (latex_cmd, preview_fn)
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
			opts = opts + ' -Ppdf -G0 -u lilypond.map'
		else:
			ly.warning (_ ('''Trying create PDF, but no PFA fonts found.
Using bitmap fonts instead. This will look bad.'''))

	cmd = 'dvips %s -o%s %s' % (opts, outbase + '.ps', outbase + '.dvi')
	ly.system (cmd)

	if preview_p:
		cmd = 'dvips -E -o%s %s' % ( outbase + '.preview.ps', outbase + '.preview.dvi')		
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
		if name in os.listdir (d):
			return os.path.join (d, name)

# Added as functionality to ly2dvi, because ly2dvi may well need to do this
# in future too.
PS = '%!PS-Adobe'
def find_pfa_fonts (name):
	s = open (name).read ()
	if s[:len (PS)] != PS:
		# no ps header?
		ly.error (_ ("not a PostScript file: `%s\'" % name))
		ly.exit (1)
	here = 0
	m = re.match ('.*?/(feta[-a-z0-9]+) +findfont', s[here:], re.DOTALL)
	pfa = []
	while m:
		here = m.end (1)
		pfa.append (m.group (1))
		m = re.match ('.*?/(feta[-a-z0-9]+) +findfont', s[here:], re.DOTALL)
	return pfa

	
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
		targets.append ('PS')
	elif o == '--pdf' or o == '-p':
		targets.append ('PS')
		targets.append ('PDF')
	elif o == '--keep' or o == '-k':
		keep_temp_dir_p = 1
	elif o == '--debug':
		verbose_p = 1
		debug_p = 1 
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
		ly.identify (sys.stdout)
		sys.exit (0)
	elif o == '--pdftex':
		latex_cmd = 'pdflatex'
		targets.remove('DVI')
		targets.append('PDFTEX')
		pdftex_p = 1
		tex_extension = '.pdftex'
	elif o == '--warranty' or o == '-w':
		status = os.system ('lilypond -w')
		if status:
			ly.warranty ()
		sys.exit (0)

# Don't convert input files to abspath, rather prepend '.' to include
# path.
include_path.insert (0, '.')

# As a neat trick, add directory part of first input file
# to include path.  That way you can do without the clumsy -I in:

#    ly2dvi -I foe/bar/baz foo/bar/baz/baz.ly
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
		if string.find (i, ' ') >= 0:
			ly.error (_ ("filename should not contain spaces: `%s'") %
			       i)
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

	# to be sure, add tmpdir *in front* of inclusion path.
	#os.environ['TEXINPUTS'] =  tmpdir + ':' + os.environ['TEXINPUTS']
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

	if 'PNG' in  targets:
		ly.make_preview (outbase)

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
				ly.warning (_("Running LaTeX falied. Rerun with --verbose for a trace."))
				

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
		
	# Hmm, if this were a function, we could call it the except: clauses
	for i in targets:
		ext = string.lower (i)
		ly.cp_to_dir ('.*\.%s$' % ext, outdir)
		outname = outbase + '.' + string.lower (i)
		abs = os.path.join (outdir, outname)
		if reldir != '.':
			outname = os.path.join (reldir, outname)
		if os.path.isfile (abs):
			ly.progress (_ ("%s output to `%s'...") % (i, outname))
			ly.progress ('\n')
		elif verbose_p:
			ly.warning (_ ("can't find file: `%s'") % outname)
			
	os.chdir (original_dir)
	ly.cleanup_temp ()

	sys.exit (lilypond_error_p)
