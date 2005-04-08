#!@PYTHON@
# vim: set noexpandtab:

"""

  TODO:
  * junk --outdir for--output
  * Figure out clean set of options.
  *
  * texinfo: add support for @pagesize

  todo: dimension handling (all the x2y) is clumsy. (tca: Thats
        because the values are taken directly from texinfo.tex,
        geometry.sty and article.cls. Give me a hint, and I'll
        fix it.)


  TODO: magnification support should also work for texinfo -> html: eg. add as option to dvips.



  This is a slightly hairy program. The general approach is as follows 
  The input string is chopped up in chunks, i.e. ,  a list of tuples

    with the format  (TAG_STR, MAIN_STR, OPTIONS, TODO, BASE)

  This list is built step by step: first ignore and verbatim commands
  are handled, delivering a list of chunks.
  
  then all chunks containing lilypond commands are chopped up

  when all chunks have their final form, all bodies from lilypond blocks are 
  extracted, and if applicable, written do disk and run through lilypond.
  

tags supported

  ignore
  lilypond
  input
  verb
  verbatim
  multicols
  numcols
  



"""

#  This is was the idea for handling of comments:
#	Multiline comments, @ignore .. @end ignore is scanned for
#	in read_doc_file, and the chunks are marked as 'ignore', so
#	lilypond-book will not touch them any more. The content of the
#	chunks are written to the output file. Also 'include' and 'input'
#	regex has to check if they are commented out.
#

#	Then it is scanned for 'lilypond', 'lilypond-file' and 'lilypond-block'.
#	These three regex's has to check if they are on a commented line,
#	% for latex, @c for texinfo.
#
#	Then lines that are commented out with % (latex) and @c (Texinfo)
#	are put into chunks marked 'ignore'. This cannot be done before
#	searching for the lilypond-blocks because % is also the comment character
#	for lilypond.
#
#	The the rest of the rexeces are searched for. They don't have to test
#	if they are on a commented out line.

import glob
import stat
import string


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
program_version = '@TOPLEVEL_VERSION@'
program_name = 'lilypond-book'
verbose_p = 0
pseudo_filter_p = 0
original_dir = os.getcwd ()


preview_resolution = 90

## FIXME
## ly2dvi: silly name?
## do -P or -p by default?
##help_summary = _ ("Run LilyPond using LaTeX for titling")
help_summary = _ ("Process LilyPond snippets in hybrid html, LaTeX or texinfo document")
copyright = ('Tom Cato Amundsen <tca@gnu.org>',
	     'Han-Wen Nienhuys <hanwen@cs.uu.nl>')

option_definitions = [
	(_ ("EXT"), 'f', 'format', _ ("use output format EXT (texi [default], texi-html, latex, html)")),
	(_ ("DIM"),  '', 'default-music-fontsize', _ ("default fontsize for music.  DIM is assumed to be in points")),
	(_ ("DIM"),  '', 'default-lilypond-fontsize', _ ("deprecated, use --default-music-fontsize")),
	(_ ("OPT"), '', 'extra-options', _ ("pass OPT quoted to the lilypond command line")),
	(_ ("DIM"), '', 'force-music-fontsize', _ ("force fontsize for all inline lilypond. DIM is assumed to be in points")),
	(_ ("DIM"), '', 'force-lilypond-fontsize', _ ("deprecated, use --force-music-fontsize")),
	('', 'h', 'help', _ ("print this help")),
	(_ ("DIR"), 'I', 'include', _ ("include path")),
	('', 'M', 'dependencies', _ ("write dependencies")),
	(_ ("PREF"), '',  'dep-prefix', _ ("prepend PREF before each -M dependency")),
	('', 'n', 'no-lily', _ ("don't run lilypond")),
	('', '', 'no-pictures', _ ("don't generate pictures")),
	('', '', 'no-music', _ ("strip all lilypond blocks from output")),
	(_ ("FILE"), 'o', 'outname', _ ("filename main output file")),
	(_ ("FILE"), '', 'outdir', _ ("where to place generated files")),
	(_ ('RES'), '', 'preview-resolution',
	 _ ("set the resolution of the preview to RES")),
	('', 'V', 'verbose', _ ("be verbose")),
	('', 'v', 'version', _ ("print version information")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

# format specific strings, ie. regex-es for input, and % strings for output

# global variables

include_path = [os.getcwd ()]

#lilypond_binary = 'valgrind --suppressions=/home/hanwen/usr/src/guile-1.6.supp  --num-callers=10 /home/hanwen/usr/src/lilypond/lily/out/lilypond'

lilypond_binary = os.path.join ('@bindir@', 'lilypond-bin')

# only use installed binary  when we're installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
	lilypond_binary = 'lilypond-bin'



ly2dvi_binary = os.path.join ('@bindir@', 'ly2dvi')

# only use installed binary  when we're installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
	ly2dvi_binary = 'ly2dvi'



g_extra_opts = ''
g_here_dir = os.getcwd ()
g_dep_prefix = ''
g_outdir = ''
g_force_music_fontsize = 0
g_do_pictures = 1
g_do_music = 1
g_make_html = 0

format = ''
g_run_lilypond = 1
no_match = 'a\ba'

default_music_fontsize = 16
default_text_fontsize = 12
paperguru = None

################################################################
# Dimension handling for LaTeX.
# 
class LatexPaper:
	def __init__ (self):
		self.m_document_preamble = []
		self.m_num_cols = 1
		self.m_multicols = 1

	def find_latex_dims (self):
		if g_outdir:
			fname = os.path.join (g_outdir, "lily-tmp.tex")
		else:
			fname = "lily-tmp.tex"
		try:
			f = open (fname, "w")
		except IOError:
			error ("Error creating temporary file '%s'" % fname)

		for s in self.m_document_preamble:
			f.write (s)
		f.write (r"""
\begin{document}
\typeout{---}
\typeout{\columnsep \the\columnsep}
\typeout{\textwidth \the\textwidth}
\typeout{---}
\end{document}
		""")
		f.close ()
		re_dim = re.compile (r"\\(\w+)\s+(\d+\.\d+)")

		cmd = "latex '\\nonstopmode \input %s'" % fname
	        # Ugh.  (La)TeX writes progress and error messages on stdout
		# Redirect to stderr
		cmd = '(( %s  >&2 ) >&- )' % cmd
		status = ly.system (cmd, ignore_error = 1)
		signal = 0xf & status
		exit_status = status >> 8
		
		if status:
			ly.error (_ ("LaTeX failed."))
			ly.error (_ ("The error log is as follows:"))
			
			#URG see ly2dvi
			try:
				lns = open ('lily-tmp.log').readlines ()
			except:
				lns = ''
			countdown = -3
			for ln in lns:
				sys.stderr.write (ln)
				if re.match ('^!', ln):
					countdown = 3

				if countdown == 0:
					break

				if countdown > 0:
					countdown = countdown -1

			sys.stderr.write ("  ... (further messages elided)...\n")
			sys.exit (1)

		lns = open ('lily-tmp.log').readlines ()
		for ln in lns:
			ln = string.strip (ln)
			m = re_dim.match (ln)
			if m:
				if m.groups ()[0] in ('textwidth', 'columnsep'):
					self.__dict__['m_%s' % m.groups ()[0]] = float (m.groups ()[1])

		try:
			os.remove (fname)
			os.remove (os.path.splitext (fname)[0]+".aux")
			os.remove (os.path.splitext (fname)[0]+".log")
		except:
			pass

		if not self.__dict__.has_key ('m_textwidth'):
			raise 'foo!'

	def get_linewidth (self):
		if self.m_num_cols == 1:
			w = self.m_textwidth
		else:
			w = (self.m_textwidth - self.m_columnsep)/2
		if self.m_multicols > 1:
			return (w - self.m_columnsep* (self.m_multicols-1)) \
			   / self.m_multicols
		return w


class HtmlPaper:
	def __init__ (self):
		self.m_papersize = 'letterpaper'
		self.m_fontsize = 12
	def get_linewidth (self):
		return html_linewidths[self.m_papersize][self.m_fontsize]

class TexiPaper:
	def __init__ (self):
		self.m_papersize = 'letterpaper'
		self.m_fontsize = 12
	def get_linewidth (self):
		return texi_linewidths[self.m_papersize][self.m_fontsize]

def mm2pt (x):
	return x * 2.8452756
def in2pt (x):
	return x * 72.26999
def em2pt (x, fontsize = 10):
	return {10: 10.00002, 11: 10.8448, 12: 11.74988}[fontsize] * x
def ex2pt (x, fontsize = 10):
	return {10: 4.30554, 11: 4.7146, 12: 5.16667}[fontsize] * x

def pt2pt (x):
	return x

dimension_conversion_dict ={
	'mm': mm2pt,
	'cm': lambda x: mm2pt (10*x),
	'in': in2pt,
	'em': em2pt,
	'ex': ex2pt,
	'pt': pt2pt
	}

# Convert numeric values, with or without specific dimension, to floats.
# Keep other strings
def conv_dimen_to_float (value):
	if type (value) == type (""):
		m = re.match ("([0-9.]+)(cm|in|pt|mm|em|ex)",value)
		if m:
			unit = m.group (2)
			num = string.atof (m.group (1))
			conv =  dimension_conversion_dict[m.group (2)]

			value = conv (num)

		elif re.match ("^[0-9.]+$",value):
			value = float (value)

	return value

texi_linewidths = {
	'afourpaper': {12: mm2pt (160)},
	'afourwide': {12: in2pt (6.5)},
	'afourlatex': {12: mm2pt (150)},
	'smallbook': {12: in2pt (5)},
	'letterpaper': {12: in2pt (6)}}

html_linewidths = {
	'afourpaper': {12: mm2pt (160)},
	'afourwide': {12: in2pt (6.5)},
	'afourlatex': {12: mm2pt (150)},
	'smallbook': {12: in2pt (5)},
	'letterpaper': {12: in2pt (6)}}


################################################################
# How to output various structures. 
output_dict= {


	'html' : {

		'output-filename' : r'''
<!-- %s >
<a href="%s">
<pre>%s</pre></a>:''',
		'output-lilypond-fragment': '''<lilypond%s>
\context Staff\context Voice{ %s }
</lilypond>''',
		'output-noinline': r'''
<!-- generated: %(fn)s.png !-->
''',
		## maybe <hr> ?
		'pagebreak': None,
		# Verbatim text is always finished with \n.  FIXME: For HTML,
		# this newline should be removed.
		'output-verbatim': r'''<pre>
%s</pre>''',
		# Verbatim text is always finished with \n.  FIXME: For HTML,
		# this newline should be removed.
		'output-small-verbatim': r'''<font size=-1><pre>
%s</pre></font>''',
		## Ugh we need to differentiate on origin:
		## lilypond-block origin wants an extra <p>, but
		## inline music doesn't.
		## possibly other center options?
		'output-html': r'''
%(htmlimages)s''',
		},


	'latex': {

		'output-lilypond-fragment' : r'''\begin[singleline,%s]{lilypond}
    \context Voice{
      %s
    }
\end{lilypond}''',
		'output-filename' : r'''\verb+%s+:\\
%% %s
%% %s
''',

		# verbatim text is always finished with \n
		'output-verbatim': r'''\begin{verbatim}
%s\end{verbatim}
''',
		# verbatim text is always finished with \n
		'output-small-verbatim': r'''{\small\begin{verbatim}
%s\end{verbatim}}
''',
		'output-default-post': "\\def\postLilyPondExample{}\n",
		'output-default-pre': "\\def\preLilyPondExample{}\n",
		'usepackage-graphics': '\\usepackage{graphics}\n',
		'output-noinline': r'''
%% generated: %(fn)s.eps
''',
		'output-latex-quoted': r'''{\preLilyPondExample
\input %(fn)s.tex
\postLilyPondExample}''',
		'output-latex-noquote': r'''{\parindent 0pt
\preLilyPondExample
\input %(fn)s.tex
\postLilyPondExample}''',
		'pagebreak': r'\pagebreak',
		},


	'texi' : {


		'output-filename' : r'''
@ifnothtml
@file{%s}:@*
@end ifnothtml
@ifhtml
@uref{%s,@file{%s}}
@end ifhtml
''',
		'output-lilypond-fragment': '''@lilypond[%s]
\context Staff\context Voice{ %s }
@end lilypond ''',
		'output-noinline': r'''
@c generated: %(fn)s.png
''',
		'pagebreak': None,
		# verbatim text is always finished with \n
		'output-small-verbatim': r'''@smallexample
%s@end smallexample
''',
		# verbatim text is always finished with \n
		'output-verbatim': r'''@example
%s@end example
''',
		# do some tweaking: @ is needed in some ps stuff.
		#
		# ugh, the <p> below breaks inline images...
		'output-texi-noquote': r'''@tex
\catcode`\@=12
\parindent 0pt
\def\lilypondbook{}
\input %(fn)s.tex
\catcode`\@=0
@end tex
@html
<p>%(htmlimages)s
<p>
@end html
''',
		'output-texi-quoted': r'''@quotation
@tex
\catcode`\@=12
\def\lilypondbook{}
\input %(fn)s.tex
\catcode`\@=0
@end tex
@html
<p>%(htmlimages)s
<p>
@end html
@end quotation
''',
		}

	}

def output_verbatim (body, small):
	global format
	if format == 'html':
		body = re.sub ('&', '&amp;', body)
		body = re.sub ('>', '&gt;', body)
		body = re.sub ('<', '&lt;', body)
	elif format == 'texi':
		# clumsy workaround for python 2.2 pre bug.
		body = re.sub ('@', '@@', body)
		body = re.sub ('{', '@{', body)
		body = re.sub ('}', '@}', body)

	if small:
		key = 'output-small-verbatim'
	else:
		key = 'output-verbatim'
	return get_output (key) % body


################################################################
# Recognize special sequences in the input 


# Warning: This uses extended regular expressions.  Tread with care.
#
# legenda
#
# (?P<name>regex) -- assign result of REGEX to NAME
# *? -- match non-greedily.
# (?m) -- multiline regex: make ^ and $ match at each line
# (?s) -- make the dot match all characters including newline
re_dict = {
	'html': {
		'include':  no_match,
		'input': no_match,
		'header': no_match,
		'preamble-end': no_match,
		'landscape': no_match,
		'verbatim': r'''(?s)(?P<code><pre>\s.*?</pre>\s)''',
		'verb': r'''(?P<code><pre>.*?</pre>)''',
		'lilypond-file': r'(?m)(?P<match><lilypondfile(?P<options>[^>]+)?>\s*(?P<filename>[^<]+)\s*</lilypondfile>)',
		'lilypond' : '(?m)(?P<match><lilypond((?P<options>[^:]*):)(?P<code>.*?)/>)',
		'lilypond-block': r'''(?ms)(?P<match><lilypond(?P<options>[^>]+)?>(?P<code>.*?)</lilypond>)''',
		'option-sep' : '\s*',
		'intertext': r',?\s*intertext=\".*?\"',
		'multiline-comment': r"(?sm)\s*(?!@c\s+)(?P<code><!--\s.*?!-->)\s",
		'singleline-comment': no_match,
		'numcols': no_match,
		'multicols': no_match,
		'ly2dvi': r'(?m)(?P<match><ly2dvifile(?P<options>[^>]+)?>\s*(?P<filename>[^<]+)\s*</ly2dvifile>)',
		},

	'latex': {
		'input': r'(?m)^[^%\n]*?(?P<match>\\mbinput{?([^}\t \n}]*))',
		'include': r'(?m)^[^%\n]*?(?P<match>\\mbinclude{(?P<filename>[^}]+)})',
		'option-sep' : ',\s*',
		'header': r"\n*\\documentclass\s*(\[.*?\])?",
		'preamble-end': r'(?P<code>\\begin\s*{document})',
		'verbatim': r"(?s)(?P<code>\\begin\s*{verbatim}.*?\\end{verbatim})",
		'verb': r"(?P<code>\\verb(?P<del>.).*?(?P=del))",
		'lilypond-file': r'(?m)^[^%\n]*?(?P<match>\\lilypondfile\s*(\[(?P<options>.*?)\])?\s*\{(?P<filename>.+)})',
		'lilypond' : r'(?m)^[^%\n]*?(?P<match>\\lilypond\s*(\[(?P<options>.*?)\])?\s*{(?P<code>.*?)})',
		'lilypond-block': r"(?sm)^[^%\n]*?(?P<match>\\begin\s*(\[(?P<options>.*?)\])?\s*{lilypond}(?P<code>.*?)\\end{lilypond})",
		'def-post-re': r"\\def\\postLilyPondExample",
		'def-pre-re': r"\\def\\preLilyPondExample",
		'usepackage-graphics': r"\usepackage\s*{graphics}",
		'intertext': r',?\s*intertext=\".*?\"',
		'multiline-comment': no_match,
		'singleline-comment': r"(?m)^.*?(?P<match>(?P<code>^%.*$\n+))",
		'numcols': r"(?P<code>\\(?P<num>one|two)column)",
		'multicols': r"(?P<code>\\(?P<be>begin|end)\s*{multicols}({(?P<num>\d+)?})?)",
		'ly2dvi': no_match,

		},

	# why do we have distinction between @mbinclude and @include?

	'texi': {
		'include':  '(?m)^[^%\n]*?(?P<match>@mbinclude\s+(?P<filename>\S*))',
		'input': no_match,
		'header': no_match,
		'preamble-end': no_match,
		'landscape': no_match,
		'verbatim': r'''(?s)(?P<code>@example\s.*?@end example\s)''',
		'verb': r'''(?P<code>@code{.*?})''',
		'lilypond-file': '(?m)^(?P<match>@lilypondfile(\[(?P<options>[^]]*)\])?{(?P<filename>[^}]+)})',
		'lilypond' : '(?m)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?{(?P<code>.*?)})',
		'lilypond-block': r'''(?ms)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?\s(?P<code>.*?)@end lilypond)\s''',
		'option-sep' : ',\s*',
		'intertext': r',?\s*intertext=\".*?\"',
		'multiline-comment': r"(?sm)^\s*(?!@c\s+)(?P<code>@ignore\s.*?@end ignore)\s",
		'singleline-comment': r"(?m)^.*?(?P<match>(?P<code>@c.*$\n+))",
		'numcols': no_match,
		'multicols': no_match,
		'ly2dvi': no_match,
		}
	}


for r in re_dict.keys ():
	olddict = re_dict[r]
	newdict = {}
	for k in olddict.keys ():
		try:
			newdict[k] = re.compile (olddict[k])
		except:
			print 'invalid regexp: %s' % olddict[k]

			## we'd like to catch and reraise a more
			## detailed error, but alas, the exceptions
			## changed across the 1.5/2.1 boundary.

			raise "Invalid re"
	re_dict[r] = newdict


def uniq (list):
	list.sort ()
	s = list
	list = []
	for x in s:
		if x not in list:
			list.append (x)
	return list


def get_output (name):
	return  output_dict[format][name]

def get_re (name):
	return  re_dict[format][name]

def bounding_box_dimensions (fname):
	if g_outdir:
		fname = os.path.join (g_outdir, fname)
	try:
		fd = open (fname)
	except IOError:
		error ("Error opening `%s'" % fname)
	str = fd.read ()
	s = re.search ('%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)', str)
	if s:

		gs = map (lambda x: string.atoi (x), s.groups ())
		return (int (gs[2] - gs[0] + 0.5),
			int (gs[3] - gs[1] + 0.5))
	else:
		return (0,0)

def error (str):
	sys.stderr.write ("\n\n" + str + "\nExiting ... \n\n")
	raise 'Exiting.'


def compose_full_body (body, opts):
	'''Construct the lilypond code to send to LilyPond.
	Add stuff to BODY using OPTS as options.'''
	music_size = default_music_fontsize
	if g_force_music_fontsize:
		music_size = g_force_music_fontsize
	indent = ''
	linewidth = ''
	notime = ''
	for o in opts:
		if not g_force_music_fontsize:
			m = re.match ('([0-9]+)pt', o)
			if m:
				music_size = string.atoi (m.group (1))

		m = re.match ('indent=([-.0-9]+)(cm|in|mm|pt)', o)
		if m:
			f = float (m.group (1))
			indent = 'indent = %f\\%s' % (f, m.group (2))

		m = re.match ('linewidth=([-.0-9]+)(cm|in|mm|pt)', o)
		if m:
			f = float (m.group (1))
			linewidth = 'linewidth = %f\\%s' % (f, m.group (2))

	if re.search ('\\\\score', body):
		is_fragment = 0
	else:
		is_fragment = 1
	if 'fragment' in opts:
		is_fragment = 1
	if 'nofragment' in opts:
		is_fragment = 0

	if is_fragment and not 'multiline' in opts:
		opts.append ('singleline')

	if 'raggedright' in opts or  'singleline' in opts:
		if not linewidth:
			linewidth = 'raggedright = ##t'
		if not indent:
			indent = 'indent = 0.0\mm'
	elif not linewidth:
		global paperguru
		l = paperguru.get_linewidth ()
		linewidth = 'linewidth = %f\pt' % l

	if 'noindent' in opts:
		indent = 'indent = 0.0\mm'

	if 'notime' in opts:
		notime = r'''
\translator {
  \StaffContext
  \remove Time_signature_engraver
}
'''

	orig_name = ''
	for o in opts:
		m= re.search ('relative(.*)', o)
		v = 0
		if m:
			try:
				v = string.atoi (m.group (1))
			except ValueError:
				pass

			v = v + 1
			pitch = 'c'
			if v < 0:
				pitch = pitch + '\,' * v
			elif v > 0:
				pitch = pitch + '\'' * v

			body = '\\relative %s { %s }' % (pitch, body)
		m =re.search ("filename=(.*)", o)
		if m:
			orig_name = m.group (1)
		
	if is_fragment:
		body = r'''
\score {
  \notes {
%s
  }
}
''' % body

	opts = uniq (opts)
	optstring = string.join (opts, ' ')
	optstring = re.sub ('\n', ' ', optstring)
	body = r'''
%% Generated automatically by: lilypond-book.py
%% options are %s
\include "paper%d.ly"
\paper  {
  %s
  %s
  %s
}
''' % (optstring, music_size, linewidth, indent, notime) + body

	if orig_name:
		body = '\\renameinput \"%s\"\n%s' % (orig_name, body)
	

	# ughUGH not original options
	return body

def scan_html_preamble (chunks):
	return

def scan_latex_preamble (chunks):
	# First we want to scan the \documentclass line
	# it should be the first non-comment line.
	# The only thing we really need to know about the \documentclass line
	# is if there are one or two columns to begin with.
	idx = 0
	while 1:
		if chunks[idx][0] == 'ignore':
			idx = idx + 1
			continue
		m = get_re ('header').match (chunks[idx][1])
		if not m:
			error ("Latex documents must start with a \documentclass command")
		if m.group (1):
			options = re.split (r',\s*', m.group (1)[1:-1])
		else:
			options = []
		if 'twocolumn' in options:
			paperguru.m_num_cols = 2
		break


	# Then we add everything before \begin{document} to
	# paperguru.m_document_preamble so that we can later write this header
	# to a temporary file in find_latex_dims() to find textwidth.
	while idx < len (chunks) and chunks[idx][0] != 'preamble-end':
		if chunks[idx] == 'ignore':
			idx = idx + 1
			continue
		paperguru.m_document_preamble.append (chunks[idx][1])
		idx = idx + 1

	if len (chunks) == idx:
		error ("Didn't find end of preamble (\\begin{document})")

	paperguru.find_latex_dims ()

def scan_texi_preamble (chunks):
	# this is not bulletproof..., it checks the first 10 chunks
	for c in chunks[:10]:
		if c[0] == 'input':
			for s in ('afourpaper', 'afourwide', 'letterpaper',
				  'afourlatex', 'smallbook'):
				if string.find (c[1], "@%s" % s) != -1:
					paperguru.m_papersize = s


def scan_preamble (chunks):
	global format
	if format == 'html':
		scan_html_preamble (chunks)
	elif format == 'latex':
		scan_latex_preamble (chunks)
	elif format == 'texi':
		scan_texi_preamble (chunks)


def completize_preamble (chunks):
	global format
	if format != 'latex':
		return chunks
	pre_b = post_b = graphics_b = None
	for chunk in chunks:
		if chunk[0] == 'preamble-end':
			break
		if chunk[0] == 'input':
			m = get_re ('def-pre-re').search (chunk[1])
			if m:
				pre_b = 1
		if chunk[0] == 'input':
			m = get_re ('def-post-re').search (chunk[1])
			if m:
				post_b = 1

		if chunk[0] == 'input':
			m = get_re ('usepackage-graphics').search (chunk[1])
			if m:
				graphics_b = 1
	x = 0
	while x < len (chunks) and   chunks[x][0] != 'preamble-end':
		x = x + 1

	if x == len (chunks):
		return chunks

	if not pre_b:
		chunks.insert (x, ('input', get_output ('output-default-pre')))
	if not post_b:
		chunks.insert (x, ('input', get_output ('output-default-post')))
	if not graphics_b:
		chunks.insert (x, ('input', get_output ('usepackage-graphics')))

	return chunks


read_files = []
def find_file (name):
	'''
	Search the include path for NAME. If found, return the (CONTENTS, PATH) of the file.
	'''

	if name == '-':
		return (sys.stdin.read (), '<stdin>')
	f = None
	nm = ''
	for a in include_path:
		try:
			nm = os.path.join (a, name)
			f = open (nm)
			global read_files
			read_files.append (nm)
			break
		except IOError:
			pass
	if f:
		sys.stderr.write ("Reading `%s'\n" % nm)
		return (f.read (), nm)
	else:
		error ("File not found `%s'\n" % name)
		return ('', '')

def do_ignore (match_object):
	return [('ignore', match_object.group ('code'))]
def do_preamble_end (match_object):
	return [('preamble-end', match_object.group ('code'))]

def make_verbatim (match_object):
	return [('verbatim', match_object.group ('code'))]

def make_verb (match_object):
	return [('verb', match_object.group ('code'))]

def do_include_file (m):
	"m: MatchObject"
	return [('input', get_output ('pagebreak'))] \
	     + read_doc_file (m.group ('filename')) \
	     + [('input', get_output ('pagebreak'))]

def do_input_file (m):
	return read_doc_file (m.group ('filename'))

def make_lilypond (m):
	if m.group ('options'):
		options = m.group ('options')
	else:
		options = ''
	return [('input', get_output ('output-lilypond-fragment') %
			(options, m.group ('code')))]

def make_lilypond_file (m):
	'''

	Find @lilypondfile{bla.ly} occurences and substitute bla.ly
	into a @lilypond .. @end lilypond block.

	'''

	if m.group ('options'):
		options = get_re ('option-sep').split (m.group ('options'))
	else:
		options = []
	(content, nm) = find_file (m.group ('filename'))
	options.append ("filename=%s" % nm)
	(path, base) = os.path.split (nm)
	
	if path not in include_path:
		include_path.append (path)

	return [('lilypond', content, options)]
	

def make_ly2dvi_block (m):
	'''

	Find <ly2dvifile .. >
	'''

	return [('ly2dvi', m.group ('filename'), m.group ('options'))]


def make_lilypond_block (m):
	if not g_do_music:
		return []

	if m.group ('options'):
		options = get_re ('option-sep').split (m.group ('options'))
	else:
		options = []
	options = filter (lambda s: s != '', options)
	return [('lilypond', m.group ('code'), options)]


def do_columns (m):
	global format
	if format != 'latex':
		return []
	if m.group ('num') == 'one':
		return [('numcols', m.group ('code'), 1)]
	if m.group ('num') == 'two':
		return [('numcols', m.group ('code'), 2)]

def do_multicols (m):
	global format
	if format != 'latex':
		return []
	if m.group ('be') == 'begin':
		return [('multicols', m.group ('code'), int (m.group ('num')))]
	else:
		return [('multicols', m.group ('code'), 1)]
	return []

def chop_chunks (chunks, re_name, func, use_match=0):
	newchunks = []
	for c in chunks:
		if c[0] == 'input':
			str = c[1]
			while str:
				m = get_re (re_name).search (str)
				if m == None:
					newchunks.append (('input', str))
					str = ''
				else:
					if use_match:
						newchunks.append (('input', str[:m.start ('match')]))
					else:
						newchunks.append (('input', str[:m.start (0)]))
				        #newchunks.extend (func (m))
					# python 1.5 compatible:
					newchunks = newchunks + func (m)
					str = str [m.end (0):]
		else:
			newchunks.append (c)
	return newchunks

def determine_format (str):
	"""

	SIDE EFFECT! This sets FORMAT and PAPERGURU

	"""
	
	global format
	if format == '':
		html = re.search ('(?i)<[dh]tml', str[:200])
		latex = re.search (r'''\\document''', str[:200])
		texi = re.search ('@node|@setfilename', str[:200])

		f = ''
		g = None

		if html and not latex and not texi:
			f = 'html'
		elif latex and not html and not texi:
			f = 'latex'
		elif texi and not html and not latex:
			f = 'texi'
		else:
			error ("can't determine format, please specify")
		format = f

	global paperguru
	if paperguru == None:
		if format == 'html':
			g = HtmlPaper ()
		elif format == 'latex':
			g = LatexPaper ()
		elif format == 'texi':
			g = TexiPaper ()

		paperguru = g


def read_doc_file (filename):
	'''Read the input file, find verbatim chunks and do \input and \include
	'''
	(str, path) = find_file (filename)
	determine_format (str)

	chunks = [('input', str)]

	# we have to check for verbatim before doing include,
	# because we don't want to include files that are mentioned
	# inside a verbatim environment
	chunks = chop_chunks (chunks, 'verbatim', make_verbatim)

	chunks = chop_chunks (chunks, 'verb', make_verb)
	chunks = chop_chunks (chunks, 'multiline-comment', do_ignore)
	#ugh fix input
	chunks = chop_chunks (chunks, 'include', do_include_file, 1)
	chunks = chop_chunks (chunks, 'input', do_input_file, 1)
	return chunks


taken_file_names = {}

def unique_file_name (body):
	return 'lily-' + `abs (hash (body))`

def schedule_lilypond_block (chunk):
	'''Take the body and options from CHUNK, figure out how the
	real .ly should look.  The .ly is written, and scheduled in
	TODO.

	Return: a single chunk.

	The chunk pertaining to the lilypond output
	has the format (TYPE_STR, MAIN_STR, OPTIONS, TODO, BASE), 
	where TODO has format [basename, extension, extension, ... ]
	'''

	(type, body, opts) = chunk
	assert type == 'lilypond'
	file_body = compose_full_body (body, opts)
	## Hmm, we should hash only lilypond source, and skip the
	## %options are ...
	## comment line
	basename = unique_file_name (file_body)
	for o in opts:
		m = re.search ('filename="(.*?)"', o)
		if m:
			basename = m.group (1)
			if not taken_file_names.has_key (basename):
				taken_file_names[basename] = 0
			else:
				taken_file_names[basename] = taken_file_names[basename] + 1
				basename = basename + "-t%i" % taken_file_names[basename]
	update_file (file_body, os.path.join (g_outdir, basename) + '.ly')
	needed_filetypes = ['tex']

	if format == 'html' or g_make_html:
		needed_filetypes.append ('eps')
		needed_filetypes.append ('png')
	if 'eps' in opts and not ('eps' in needed_filetypes):
		needed_filetypes.append ('eps')

	pathbase = os.path.join (g_outdir, basename)
	def must_rebuild (base, ext1, ext2):
		
		f2 = base + ext2
		f1 = base + ext1
		fp2 = base + '-page1' + ext2

		isfile2 = os.path.isfile (f2)
		
		if not isfile2 and os.path.isfile (fp2):
			f2  = fp2
			isfile2 = os.path.isfile (fp2)
			
		if (os.path.isfile (f2) and isfile2 and
		    os.stat (f1)[stat.ST_MTIME] >
		    os.stat (f2)[stat.ST_MTIME]) or \
		    not isfile2:
		
			return 1
		
	todo = []
	if 'tex' in needed_filetypes and must_rebuild (pathbase, '.ly', '.tex'):
		todo.append ('tex')
	if 'eps' in needed_filetypes and must_rebuild (pathbase, '.tex', '.eps'):
		todo.append ('eps')
	if 'png' in needed_filetypes and must_rebuild (pathbase, '.eps', '.png'):
		todo.append ('png')

	return ('lilypond', body, opts, todo, basename)

def format_lilypond_block (chunk):
	"""
	
	Figure out  what should be left MAIN_STR (meant
	for the main file) from a lilypond chunk: process
	verbatim, and other options. Return: multiple chunks.

	
	"""

	
	return_chunks = []

	(type, body, opts, todo, basename) = chunk
	assert type == 'lilypond'


	newbody = ''
	filename_chunk = None 
	if 'printfilename' in opts:
		for o in opts:
			m= re.match ("filename=(.*)", o)
			if m:
				template = get_output ("output-filename")
				b =  basename + '.ly'
				human_base = os.path.basename (m.group (1))
						  
				## todo: include path, but strip 
				## first part of the path.
				filename_chunk = ('input',  template % (human_base, b,human_base))
				break


	if 'smallverbatim' in opts:
		newbody += output_verbatim (body, 1)
	elif 'verbatim' in opts:
		newbody += output_verbatim (body, 0)

	for o in opts:
		m = re.search ('intertext="(.*?)"', o)
		if m:
			newbody = newbody + "\n"
			if format == 'texi':
				newbody = newbody + "@noindent\n"
			elif format == 'latex':
				newbody = newbody + "\\noindent\n"
			newbody = newbody + m.group (1) + "\n"

	if 'noinline' in opts:
		s = 'output-noinline'
	elif format == 'latex':
		if 'quote' in opts:
			s = 'output-latex-quoted'
		else:
			s = 'output-latex-noquote'
	elif format == 'texi':
		if 'quote' in opts:
			s = 'output-texi-quoted'
		else:
			s = 'output-texi-noquote'
	else: # format == 'html'
		s = 'output-html'

	def html_pages (basename):
		pat = os.path.join (g_outdir, "%s-page*.png"%  basename)
		
		files =  glob.glob (pat)
		
		
		template = '''<img align="center" valign="center"
		border="0" src="%s" alt="[picture of music]">'''

		str = ''
		if  files == []:
			files = [basename+'.png' ]
		else:
			files = map (os.path.basename, files)
			
		for f in  files:
			str += template % f

		str = '<a href="%s.ly">%s</a>' % (basename, str)

		return str

	
	newbody = newbody + get_output (s) % {'fn': basename,
					      'htmlimages': html_pages(basename)
					      }

	if filename_chunk:
		return_chunks += [filename_chunk]
	
	return_chunks += [('lilypond', newbody, opts, todo, basename)]
	
	return return_chunks

def format_lilypond_output_bodies (chunks):
	newchunks = []
	for c in chunks:

		if c[0] == 'lilypond':
			newchunks += format_lilypond_block (c)
		else:
			newchunks.append (c)

	return newchunks



def process_lilypond_blocks (chunks):#ugh rename
	newchunks = []
	# Count sections/chapters.
	for c in chunks:
		if c[0] == 'lilypond':
			c = schedule_lilypond_block (c)
		elif c[0] == 'numcols':
			paperguru.m_num_cols = c[2]
		elif c[0] == 'multicols':
			paperguru.m_multicols = c[2]
			
		newchunks.append (c)
		
	return newchunks

def process_ly2dvi_blocks (chunks):
	
	def process_ly2dvi_block (chunk):
		"""

Run ly2dvi script on filename specified in CHUNK.
This is only supported for HTML output.

In HTML output it will leave a download menu with ps/pdf/midi etc.  in
a separate HTML file, and a title + preview in the main html file,
linking to the menu.

		"""
		(tag, name, opts) = chunk
		assert format == 'html'
		(content, original_name) = find_file (name)

		original_name = os.path.basename (original_name)
		
		base = unique_file_name (content)
		outname = base + '.ly'
		changed = update_file (content, outname)

		preview = base + ".preview.png"
		preview_page = base + '-page1.png'
		
		if changed or not (os.path.isfile (preview) or
				   os.path.isfile (preview_page)):
			
			ly.system ('%s --preview --postscript --verbose %s ' % (ly2dvi_binary, base) ) 

			ly.make_ps_images (base + '.ps')
			ly.system ('gzip -9 - < %s.ps > %s.ps.gz' %  (base, base))
			
		def size_str (fn):
			b = os.stat(fn)[stat.ST_SIZE]
			if b < 1024:
				return '%d bytes' % b
			elif b < (2 << 20):
				return '%d kb' % (b >> 10)
			else:
				return '%d mb' % (b >> 20)

		exts = {
			'pdf' : "Print (PDF, %s)",
			'ps.gz' : "Print (gzipped PostScript, %s)",
			'png' : "View (PNG, %s)",
			'midi' : "Listen (MIDI, %s)",
			'ly' : "View source code (%s)", 
			}

		menu = ''
		page_files = glob.glob ('%s-page*.png' % base)

		for p in page_files:
			p = p.strip()
			if os.path.isfile (p):
				sz = size_str (p)
				page = re.sub ('.*page([0-9])+.*', 'View page \\1 (PNG picture, %s)\n', p)
				page = page % sz
				menu += '<li><a href="%s">%s</a>' % (p, page) 

		ext_order = ['ly', 'pdf', 'ps.gz', 'midi']
		for e in ext_order:
			fn =   base +  '.' + e
			print 'checking,' , fn
			if not os.path.isfile (fn):
				continue

			entry = exts[e] % size_str (fn)

			## TODO: do something like
			## this for texinfo/latex as well ?
			
			menu += '<li><a href="%s">%s</a>\n\n' % (fn, entry)


		explanatory_para = """The pictures are 90dpi
anti-aliased snapshots of the printed output, in PNG format. Both  PDF and PS
use scalable fonts and should look OK at any resolution."""
		
		separate_menu =r'''
<title>LilyPond example %s</title>

<h1>%s</h1>
<p><img src="%s">
<p>%s
<p>
<ul>%s</ul>''' % (original_name,original_name, preview, explanatory_para, menu)
		
		open (base + '.html','w'). write (separate_menu)

		inline_menu = '<p/><a href="%s.html"><img alt="%s" src="%s"></a><p/>' % (base, original_name, preview)

		return ('ly2dvi', inline_menu)

	newchunks = []
	for c in chunks:
		if c[0] == 'ly2dvi':
			c = process_ly2dvi_block (c)
		newchunks.append (c)

	return newchunks

def compile_all_files (chunks):
	global foutn
	eps = []
	tex = []
	png = []

	for c in chunks:
		if c[0] != 'lilypond':
			continue

		base  = c[4]
		exts = c[3]
		for e in exts:
			if e == 'eps':
				eps.append (base)
			elif e == 'tex':
				#ugh
				if base + '.ly' not in tex:
					tex.append (base + '.ly')
			elif e == 'png' and g_do_pictures:
				png.append (base)
	d = os.getcwd ()
	if g_outdir:
		os.chdir (g_outdir)
	if tex:
		# fixme: be sys-independent.
		def incl_opt (x):
			if g_outdir and x[0] != '/' :
				x = os.path.join (g_here_dir, x)
			return ' -I %s' % x

		incs = map (incl_opt, include_path)
		lilyopts = string.join (incs)
		if do_deps:
			lilyopts += ' --dependencies'
			if g_outdir:
				lilyopts += ' --dep-prefix=' + g_outdir + '/'
		lilyopts += ' --header=texidoc'
		texfiles = string.join (tex)
		cmd = string.join ((lilypond_binary, lilyopts, g_extra_opts,
				    texfiles))

		ly.lilypond_version_check (lilypond_binary, '@TOPLEVEL_VERSION@')
		
		ly.system (cmd, ignore_error = 0, progress_p = 1)

		#
		# Ugh, fixing up dependencies for .tex generation
		#
		if do_deps:
			depfiles=map (lambda x: re.sub ('(.*)\.ly', '\\1.dep',
							x), tex)

			for i in depfiles:
				f =open (i)
				text=f.read ()
				f.close ()
				text=re.sub ('\n([^:\n]*):',
					     '\n' + foutn + ':', text)
				f = open (i, 'w')
				f.write (text)
				f.close ()

	def to_eps (file):
		cmd = r"latex '\nonstopmode \input %s'" % file
	        # Ugh.  (La)TeX writes progress and error messages on stdout
		# Redirect to stderr
		cmd = '(( %s  >&2 ) >&- )' % cmd
		
		ly.system (cmd)
		ly.system ("dvips -E -o %s.eps %s" % (file, file))
	map (to_eps, eps)

	map (ly.make_ps_images, map (lambda x: x + '.eps', png))
	os.chdir (d)


def update_file (body, name):
	'''
	write the body if it has changed. Return whether BODY has changed.
	'''
	same = 0
	try:
		f = open (name)
		fs = f.read (-1)
		same = (fs == body)
	except:
		pass

	if not same:
		f = open (name , 'w')
		f.write (body)
		f.close ()

	return not same


def write_deps (fn, target, chunks):
	global read_files
	sys.stderr.write ('Writing `%s\'\n' % os.path.join (g_outdir, fn))
	f = open (os.path.join (g_outdir, fn), 'w')
	f.write ('%s%s: ' % (g_dep_prefix, target))
	for d in read_files:
		f.write ('%s ' %  d)


	## There used to be code to write .tex dependencies, but
	## that is silly: lilypond-book has its own dependency scheme
	## to ensure that all lily-XXX.tex files are there
		

	f.write ('\n')
	f.close ()
	read_files = []

def check_texidoc (chunks):
	## TODO: put file name in front of texidoc. 
	##
	n = []
        for c in chunks:
	        if c[0] == 'lilypond':
			(type, body, opts, todo, basename) = c;
			pathbase = os.path.join (g_outdir, basename)
			if os.path.isfile (pathbase + '.texidoc') \
			   and 'notexidoc' not in opts:
				n.append( ('input', '\n@include %s.texidoc\n\n' % basename))
		n.append (c)
	return n


## what's this? Docme --hwn
##
def fix_epswidth (chunks):
	newchunks = []
	for c in chunks:
		if c[0] != 'lilypond' or 'eps' not in c[2]:
			newchunks.append (c)
			continue

		mag = 1.0
		for o in c[2]:
			m  = re.match ('magnification=([0-9.]+)', o)
			if m:
				mag = string.atof (m.group (1))

		def replace_eps_dim (match, lmag = mag):
			filename = match.group (1)
			dims = bounding_box_dimensions (filename)

			return '%fpt' % (dims[0] *lmag)

		body = re.sub (r'''\\lilypondepswidth{(.*?)}''', replace_eps_dim, c[1])
		newchunks.append (('lilypond', body, c[2], c[3], c[4]))

	return newchunks


##docme: why global?
foutn=""

def do_file (input_filename):
	chunks = read_doc_file (input_filename)
	chunks = chop_chunks (chunks, 'ly2dvi', make_ly2dvi_block, 1)
	chunks = chop_chunks (chunks, 'lilypond', make_lilypond, 1)
	chunks = chop_chunks (chunks, 'lilypond-file', make_lilypond_file, 1)
	chunks = chop_chunks (chunks, 'lilypond-block', make_lilypond_block, 1)
	chunks = chop_chunks (chunks, 'singleline-comment', do_ignore, 1)
	chunks = chop_chunks (chunks, 'preamble-end', do_preamble_end)
	chunks = chop_chunks (chunks, 'numcols', do_columns)
	chunks = chop_chunks (chunks, 'multicols', do_multicols)
	
	scan_preamble (chunks)
	chunks = process_lilypond_blocks (chunks)
	chunks = process_ly2dvi_blocks (chunks)
	
	# Do It.
	global g_run_lilypond
	if g_run_lilypond:
		compile_all_files (chunks)
		chunks = fix_epswidth (chunks)


	chunks = format_lilypond_output_bodies (chunks)
	global format
	if format == 'texi':
		chunks = check_texidoc (chunks)


	x = 0
	chunks = completize_preamble (chunks)

	global foutn

	if outname:
		my_outname = outname
	elif input_filename == '-' or input_filename == "/dev/stdin":
		my_outname = '-'
	else:
		my_outname = os.path.basename (os.path.splitext (input_filename)[0]) + '.' + format
	my_depname = my_outname + '.dep'

	if my_outname == '-' or my_outname == '/dev/stdout':
		fout = sys.stdout
		foutn = "<stdout>"
		global do_deps
		do_deps = 0
	else:
		foutn = os.path.join (g_outdir, my_outname)
		sys.stderr.write ("Writing `%s'\n" % foutn)
		fout = open (foutn, 'w')
	for c in chunks:
		fout.write (c[1])
	fout.close ()
	# should chmod -w

	if do_deps:
		write_deps (my_depname, foutn, chunks)

outname = ''
try:
	(sh, long) = ly.getopt_args (option_definitions)
	(options, files) = getopt.getopt (sys.argv[1:], sh, long)
	
except getopt.error, msg:
	sys.stderr.write ('\n')
	ly.error (_ ("getopt says: `%s\'" % msg))
	sys.stderr.write ('\n')
	ly.help ()
	ly.exit (2)

do_deps = 0
for opt in options:
	o = opt[0]
	a = opt[1]

	if o == '--include' or o == '-I':
		include_path.append (a)
	elif o == '--version' or o == '-v':
		ly.identify (sys.stdout)
		sys.exit (0)
	elif o == '--verbose' or o == '-V':
		verbose_p = 1
	elif o == '--format' or o == '-f':
		format = a
		if a == 'texi-html':
			format = 'texi'
			g_make_html = 1
	elif o == '--outname' or o == '-o':
		if len (files) > 1:
			#HACK
			sys.stderr.write ("lilypond-book is confused by --outname on multiple files")
			sys.exit (1)
		outname = a
	elif o == '--help' or o == '-h':
		ly.help ()
		sys.exit (0)
	elif o == '--no-lily' or o == '-n':
		g_run_lilypond = 0
	elif o == '--preview-resolution':
		preview_resolution = string.atoi (a)
	elif o == '--dependencies' or o == '-M':
		do_deps = 1
	elif o == '--default-music-fontsize':
		default_music_fontsize = string.atoi (a)
	elif o == '--default-lilypond-fontsize':
		print "--default-lilypond-fontsize is deprecated, use --default-music-fontsize"
		default_music_fontsize = string.atoi (a)
	elif o == '--extra-options':
		g_extra_opts = a
	elif o == '--force-music-fontsize':
		g_force_music_fontsize = string.atoi (a)
	elif o == '--force-lilypond-fontsize':
		print "--force-lilypond-fontsize is deprecated, use --default-lilypond-fontsize"
		g_force_music_fontsize = string.atoi (a)
	elif o == '--dep-prefix':
		g_dep_prefix = a
	elif o == '--no-pictures':
		g_do_pictures = 0
	elif o == '--no-music':
		g_do_music = 0
	elif o == '--outdir':
		g_outdir = a
	elif o == '--warranty' or o == '-w':
		#status = os.system ('lilypond -w')
		if 1 or status:
			ly.warranty ()
		sys.exit (0)

ly.identify (sys.stderr)

if g_outdir:
	if os.path.isfile (g_outdir):
		error ("outdir is a file: %s" % g_outdir)
	if not os.path.exists (g_outdir):
		os.mkdir (g_outdir)
		
if not files:
	ly.help ()
	ly.error (_ ("no files specified on command line"))
	ly.exit (2)

ly.setup_environment ()


for input_filename in files:
	do_file (input_filename)


#
# Petr, ik zou willen dat ik iets zinvoller deed,
# maar wat ik kan ik doen, het verandert toch niets?
#   --hwn 20/aug/99
