#!@PYTHON@
# vim: set noexpandtab:
# TODO:
# * junk --outdir for --output 
# * Figure out clean set of options.
# * 
# * EndLilyPondOutput is def'd as vfil. Causes large white gaps.
# * texinfo: add support for @pagesize

# todo: dimension handling (all the x2y) is clumsy. (tca: Thats
#       because the values are taken directly from texinfo.tex,
#       geometry.sty and article.cls. Give me a hint, and I'll
#       fix it.)

#
# TODO: magnification support should also work for texinfo -> html: eg. add as option to dvips. 
# 

# This is was the idea for handling of comments:
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



import os
import stat
import string
import getopt
import sys
import __main__

# Handle bug in Python 1.6-2.1
#
# there are recursion limits for some patterns in Python 1.6 til 2.1. 
# fix this by importing the 1.5.2 implementation pre instead. Fix by Mats.

if float (sys.version[0:3]) < 2.2:
	try:
		import pre
		re = pre
		del pre
	except ImportError:
		import re
else:
	import re

# Attempt to fix problems with limited stack size set by Python!
# Sets unlimited stack size. Note that the resource module only
# is available on UNIX.
try:
       import resource
       resource.setrlimit (resource.RLIMIT_STACK, (-1, -1))
except:
       pass

errorport = sys.stderr
verbose_p = 0



try:
	import gettext
	gettext.bindtextdomain ('lilypond', localedir)
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

def progress (s):
	errorport.write (s + '\n')


program_version = '@TOPLEVEL_VERSION@'
if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.5.53'

# if set, LILYPONDPREFIX must take prevalence
# if datadir is not set, we're doing a build and LILYPONDPREFIX 
datadir = '@local_lilypond_datadir@'

if os.environ.has_key ('LILYPONDPREFIX') :
	datadir = os.environ['LILYPONDPREFIX']
else:
	datadir = '@local_lilypond_datadir@'

while datadir[-1] == os.sep:
	datadir= datadir[:-1]

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
			val = val + os.pathsep + os.environ[key]
		os.environ[key] = val

	for key in non_path_environment.keys ():
		val = non_path_environment[key]
		os.environ[key] = val

include_path = [os.getcwd()]


# g_ is for global (?)
g_extra_opts = ''
g_here_dir = os.getcwd ()
g_dep_prefix = ''
g_outdir = ''
g_force_music_fontsize = 0
g_read_lys = 0
g_do_pictures = 1
g_do_music = 1

format = ''
g_run_lilypond = 1
no_match = 'a\ba'

default_music_fontsize = 16
default_text_fontsize = 12
paperguru = None

class LatexPaper:
	def __init__(self):
		self.m_document_preamble = []
		self.m_num_cols = 1
		self.m_multicols = 1
	def find_latex_dims(self):
		if g_outdir:
			fname = os.path.join(g_outdir, "lily-tmp.tex")
		else:
			fname = "lily-tmp.tex"
		try:
			f = open(fname, "w")
		except IOError:
			error ("Error creating temporary file '%s'" % fname)
		for s in self.m_document_preamble:
			f.write(s)
		f.write(r"""
\begin{document}
\typeout{---}
\typeout{\columnsep \the\columnsep}
\typeout{\textwidth \the\textwidth}
\typeout{---}
\end{document}
		""")
		f.close()
		re_dim = re.compile(r"\\(\w+)\s+(\d+\.\d+)")
		p = os.popen("latex %s" % fname)
		ln = p.readline()
		while ln:
			ln = string.strip(ln)
			m = re_dim.match(ln)
			if m:
				if m.groups()[0] in ('textwidth', 'columnsep'):
					self.__dict__['m_%s' % m.groups()[0]] = float(m.groups()[1])
			ln = p.readline()
		try:
			os.remove (fname)
			os.remove (os.path.splitext(fname)[0]+".aux")
			os.remove (os.path.splitext(fname)[0]+".log")
		except:
			pass
	def get_linewidth(self):
		if self.m_num_cols == 1:
			w = self.m_textwidth
		else:
			w = (self.m_textwidth - self.m_columnsep)/2
		if self.m_multicols > 1:
			return (w - self.m_columnsep*(self.m_multicols-1)) \
			   / self.m_multicols
		return w


class HtmlPaper:
	def __init__(self):
		self.m_papersize = 'letterpaper'
		self.m_fontsize = 12
	def get_linewidth(self):
		return html_linewidths[self.m_papersize][self.m_fontsize]

class TexiPaper:
	def __init__(self):
		self.m_papersize = 'letterpaper'
		self.m_fontsize = 12
	def get_linewidth(self):
		return texi_linewidths[self.m_papersize][self.m_fontsize]

def mm2pt(x):
	return x * 2.8452756
def in2pt(x):
	return x * 72.26999
def em2pt(x, fontsize = 10):
	return {10: 10.00002, 11: 10.8448, 12: 11.74988}[fontsize] * x
def ex2pt(x, fontsize = 10):
	return {10: 4.30554, 11: 4.7146, 12: 5.16667}[fontsize] * x

def pt2pt(x):
	return x

dimension_conversion_dict ={
	'mm': mm2pt,
	'cm': lambda x: mm2pt(10*x),
	'in': in2pt,
	'em': em2pt,
	'ex': ex2pt,
	'pt': pt2pt
	}

# Convert numeric values, with or without specific dimension, to floats.
# Keep other strings
def conv_dimen_to_float(value):
	if type(value) == type(""):
		m = re.match ("([0-9.]+)(cm|in|pt|mm|em|ex)",value)
		if m:
			unit = m.group (2)
			num = string.atof(m.group (1))
			conv =  dimension_conversion_dict[m.group(2)]
			
			value = conv(num)
	 	
		elif re.match ("^[0-9.]+$",value):
			value = float(value)

	return value

texi_linewidths = {
	'afourpaper': {12: mm2pt(160)},
	'afourwide': {12: in2pt(6.5)},
	'afourlatex': {12: mm2pt(150)},
	'smallbook': {12: in2pt(5)},
	'letterpaper': {12: in2pt(6)}}

html_linewidths = {
	'afourpaper': {12: mm2pt(160)},
	'afourwide': {12: in2pt(6.5)},
	'afourlatex': {12: mm2pt(150)},
	'smallbook': {12: in2pt(5)},
	'letterpaper': {12: in2pt(6)}}

option_definitions = [
	('EXT', 'f', 'format', 'use output format EXT (texi [default], latex, html)'),
	('DIM',  '', 'default-music-fontsize', 'default fontsize for music.  DIM is assumed to be in points'),
	('DIM',  '', 'default-lilypond-fontsize', 'deprecated, use --default-music-fontsize'),
	('OPT', '', 'extra-options' , 'Pass OPT quoted to the lilypond command line'),
	('DIM', '', 'force-music-fontsize', 'force fontsize for all inline lilypond. DIM is assumed be to in points'),
	('DIM', '', 'force-lilypond-fontsize', 'deprecated, use --force-music-fontsize'),
	('', 'h', 'help', 'this help'),
	('DIR', 'I', 'include', 'include path'),
	('', 'M', 'dependencies', 'write dependencies'),
	('PREF', '',  'dep-prefix', 'prepend PREF before each -M dependency'),
	('', 'n', 'no-lily', 'don\'t run lilypond'),
	('', '', 'no-pictures', "don\'t generate pictures"),
	('', '', 'no-music', "strip all lilypond blocks from output"),	
	('', '', 'read-lys', "don't write ly files."),
	('FILE', 'o', 'outname', 'filename main output file'),
	('FILE', '', 'outdir', "where to place generated files"),
	('', 'V', 'verbose', 'verbose' ),
	('', 'v', 'version', 'print version information' ),
	]

# format specific strings, ie. regex-es for input, and % strings for output
output_dict= {
	'html' : {'output-lilypond': '''<lilypond%s>
%s
</lilypond>''',
		'output-filename' : r'''

<pre>%s</pre>:''',	  
		  'output-lilypond-fragment': '''<lilypond%s>
\context Staff\context Voice{ %s }
</lilypond>''',
		  'output-noinline': r'''
<!-- generated: %(fn)s.png !-->
''',
		  ## maybe <hr> ?
		  'pagebreak': None,
		  'output-verbatim': r'''<pre>
%s
</pre>''',
  		  'output-small-verbatim': r'''<font size=-1><pre>
%s
</pre></font>''',

		  ## Ugh we need to differentiate on origin:
		  ## lilypond-block origin wants an extra <p>, but
		  ## inline music doesn't.
		  ## possibly other center options?
		  'output-all': r'''
<a href="%(fn)s.png">
<img align="center" valign="center" border="0" src="%(fn)s.png" alt="[picture of music]"></a>
''',
		  },
	'latex': {
		'output-lilypond-fragment' : r'''\begin[eps,singleline,%s]{lilypond}
  \context Staff <
    \context Voice{
      %s
    }
  >
\end{lilypond}''',
		'output-filename' : r'''

\verb+%s+:''',
		'output-lilypond': r'''\begin[%s]{lilypond}
%s
\end{lilypond}
''',
		'output-verbatim': r'''\begin{verbatim}%s\end{verbatim}%%
''',
		'output-small-verbatim': r'''{\small\begin{verbatim}%s\end{verbatim}}%%''',
		'output-default-post': "\\def\postLilypondExample{}\n",
		'output-default-pre': "\\def\preLilypondExample{}\n",
		'usepackage-graphics': '\\usepackage{graphics}\n',
		'output-eps': '\\noindent\\parbox{\\lilypondepswidth{%(fn)s.eps}}{\includegraphics{%(fn)s.eps}}',
		'output-noinline': r'''
%% generated: %(fn)s.eps
''',
		'output-tex': '{\\preLilypondExample \\input %(fn)s.tex \\postLilypondExample\n}',
		'pagebreak': r'\pagebreak',
		},
	
	'texi' : {'output-lilypond': '''@lilypond[%s]
%s
@end lilypond 
''',
		'output-filename' : r'''

@file{%s}:''',	  
		  'output-lilypond-fragment': '''@lilypond[%s]
\context Staff\context Voice{ %s }
@end lilypond ''',
		  'output-noinline': r'''
@c generated: %(fn)s.png		  
''',
		  'pagebreak': None,
		  'output-small-verbatim': r'''@smallexample
%s
@end smallexample
''',
		  'output-verbatim': r'''@example
%s
@end example
''',

# do some tweaking: @ is needed in some ps stuff.
# override EndLilyPondOutput, since @tex is done
# in a sandbox, you can't do \input lilyponddefs at the
# top of the document.

# should also support fragment in

# ugh, the <p> below breaks inline images...
		  
		  'output-all': r'''
@tex
\catcode`\@=12
\input lilyponddefs
\def\EndLilyPondOutput{}
\input %(fn)s.tex
\catcode`\@=0
@end tex
@html
<p>
<a href="%(fn)s.png">
<img border=0 src="%(fn)s.png" alt="[picture of music]">
</a>
@end html
''',
		}
	
	}

def output_verbatim (body, small):
	if __main__.format == 'html':
		body = re.sub ('&', '&amp;', body)
		body = re.sub ('>', '&gt;', body)
		body = re.sub ('<', '&lt;', body)
	elif __main__.format == 'texi':
		body = re.sub ('([@{}])', '@\\1', body)

	if small:
		key = 'output-small-verbatim'
	else:
		key = 'output-verbatim'
	return get_output (key) % body


#warning: this uses extended regular expressions. Tread with care.

# legenda

# (?P  -- name parameter
# *? -- match non-greedily.
# (?m)  -- ?  
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
		 },
	
	'latex': {'input': r'(?m)^[^%\n]*?(?P<match>\\mbinput{?([^}\t \n}]*))',
		  'include': r'(?m)^[^%\n]*?(?P<match>\\mbinclude{(?P<filename>[^}]+)})',
		  'option-sep' : ',\s*',
		  'header': r"\n*\\documentclass\s*(\[.*?\])?",
		  'preamble-end': r'(?P<code>\\begin{document})',
		  'verbatim': r"(?s)(?P<code>\\begin{verbatim}.*?\\end{verbatim})",
		  'verb': r"(?P<code>\\verb(?P<del>.).*?(?P=del))",
		  'lilypond-file': r'(?m)^[^%\n]*?(?P<match>\\lilypondfile\s*(\[(?P<options>.*?)\])?\s*\{(?P<filename>.+)})',
		  'lilypond' : r'(?m)^[^%\n]*?(?P<match>\\lilypond\s*(\[(?P<options>.*?)\])?\s*{(?P<code>.*?)})',
		  'lilypond-block': r"(?sm)^[^%\n]*?(?P<match>\\begin\s*(\[(?P<options>.*?)\])?\s*{lilypond}(?P<code>.*?)\\end{lilypond})",
		  'def-post-re': r"\\def\\postLilypondExample",
		  'def-pre-re': r"\\def\\preLilypondExample",
		  'usepackage-graphics': r"\usepackage{graphics}",
		  'intertext': r',?\s*intertext=\".*?\"',
		  'multiline-comment': no_match,
		  'singleline-comment': r"(?m)^.*?(?P<match>(?P<code>^%.*$\n+))",
		  'numcols': r"(?P<code>\\(?P<num>one|two)column)",
		  'multicols': r"(?P<code>\\(?P<be>begin|end){multicols}({(?P<num>\d+)?})?)",
		  },


	# why do we have distinction between @mbinclude and @include?

	
	'texi': {
		 'include':  '(?m)^[^%\n]*?(?P<match>@mbinclude[ \n\t]+(?P<filename>[^\t \n]*))',
		 'input': no_match,
		 'header': no_match,
		 'preamble-end': no_match,
		 'landscape': no_match,
		 'verbatim': r'''(?s)(?P<code>@example\s.*?@end example\s)''',
		 'verb': r'''(?P<code>@code{.*?})''',
		 'lilypond-file': '(?m)^(?P<match>@lilypondfile(\[(?P<options>[^]]*)\])?{(?P<filename>[^}]+)})',
		 'lilypond' : '(?m)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?{(?P<code>.*?)})',
		 'lilypond-block': r'''(?ms)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?\s(?P<code>.*?)@end +lilypond)\s''',
		 'option-sep' : ',\s*',
		 'intertext': r',?\s*intertext=\".*?\"',
		 'multiline-comment': r"(?sm)^\s*(?!@c\s+)(?P<code>@ignore\s.*?@end ignore)\s",
		 'singleline-comment': r"(?m)^.*?(?P<match>(?P<code>@c.*$\n+))",
		 'numcols': no_match,
		 'multicols': no_match,
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

			# we'd like to catch and reraise a more detailed  error, but
			# alas, the exceptions changed across the 1.5/2.1 boundary.
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

def bounding_box_dimensions(fname):
	if g_outdir:
		fname = os.path.join(g_outdir, fname)
	try:
		fd = open(fname)
	except IOError:
		error ("Error opening `%s'" % fname)
	str = fd.read ()
	s = re.search('%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)', str)
	if s:
		
		gs = map (lambda x: string.atoi (x), s.groups ())
		return (int (gs[2] - gs[0] + 0.5),
			int (gs[3] - gs[1] + 0.5))
	else:
		return (0,0)

def error (str):
	sys.stderr.write (str + "\n  Exiting ... \n\n")
	raise 'Exiting.'


def compose_full_body (body, opts):
	'''Construct the lilypond code to send to Lilypond.
	Add stuff to BODY using OPTS as options.'''
	music_size = default_music_fontsize
	if g_force_music_fontsize:
		music_size = g_force_music_fontsize
	indent = ''
	linewidth = ''
	for o in opts:
		if not g_force_music_fontsize:
			m = re.match ('([0-9]+)pt', o)
			if m:
				music_size = string.atoi(m.group (1))

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
		opts.append('singleline')
		
	if 'singleline' in opts:
		linewidth = 'linewidth = -1.0'
	elif not linewidth:
		l = __main__.paperguru.get_linewidth ()
		linewidth = 'linewidth = %f\pt' % l

	if 'noindent' in opts:
		indent = 'indent = 0.0\mm'

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

			body = '\\relative %s { %s }' %(pitch, body)
	
	if is_fragment:
		body = r'''\score { 
 \notes { %s }
  \paper { }  
}''' % body

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
} 
''' % (optstring, music_size, linewidth, indent) + body

	# ughUGH not original options
	return body

def scan_html_preamble (chunks):
	return

def scan_latex_preamble(chunks):
	# First we want to scan the \documentclass line
	# it should be the first non-comment line.
	# The only thing we really need to know about the \documentclass line
	# is if there are one or two columns to begin with.
	idx = 0
	while 1:
		if chunks[idx][0] == 'ignore':
			idx = idx + 1
			continue
		m = get_re ('header').match(chunks[idx][1])
		if not m:
			error ("Latex documents must start with a \documentclass command")
		if m.group (1):
			options = re.split (',[\n \t]*', m.group(1)[1:-1])
		else:
			options = []
		if 'twocolumn' in options:
			paperguru.m_num_cols = 2
		break

	# Then we add everythin before \begin{document} to
	# paperguru.m_document_preamble so that we can later write this header
	# to a temporary file in find_latex_dims() to find textwidth.
	while idx < len(chunks) and chunks[idx][0] != 'preamble-end':
		if chunks[idx] == 'ignore':
			idx = idx + 1
			continue
		paperguru.m_document_preamble.append(chunks[idx][1])
		idx = idx + 1
	paperguru.find_latex_dims()

def scan_texi_preamble (chunks):
	# this is not bulletproof..., it checks the first 10 chunks
	for c in chunks[:10]:
		if c[0] == 'input':
			for s in ('afourpaper', 'afourwide', 'letterpaper',
				  'afourlatex', 'smallbook'):
				if string.find(c[1], "@%s" % s) != -1:
					paperguru.m_papersize = s


def scan_preamble (chunks):
	if __main__.format == 'html':
		scan_html_preamble (chunks)
	elif __main__.format == 'latex':
		scan_latex_preamble (chunks)
	elif __main__.format == 'texi':
		scan_texi_preamble (chunks)
		

def completize_preamble (chunks):
	if __main__.format != 'latex':
		return chunks
	pre_b = post_b = graphics_b = None
	for chunk in chunks:
		if chunk[0] == 'preamble-end':
			break
		if chunk[0] == 'input':
			m = get_re('def-pre-re').search(chunk[1])
			if m:
				pre_b = 1
		if chunk[0] == 'input':
			m = get_re('def-post-re').search(chunk[1])
			if m:
				post_b = 1
				
		if chunk[0] == 'input':
			m = get_re('usepackage-graphics').search(chunk[1])
			if m:
				graphics_b = 1
	x = 0
	while x < len (chunks) and   chunks[x][0] != 'preamble-end':
		x = x + 1

	if x == len(chunks):
		return chunks
	
	if not pre_b:
		chunks.insert(x, ('input', get_output ('output-default-pre')))
	if not post_b:
		chunks.insert(x, ('input', get_output ('output-default-post')))
	if not graphics_b:
		chunks.insert(x, ('input', get_output ('usepackage-graphics')))

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

def do_ignore(match_object):
	return [('ignore', match_object.group('code'))]
def do_preamble_end(match_object):
	return [('preamble-end', match_object.group('code'))]

def make_verbatim(match_object):
	return [('verbatim', match_object.group('code'))]

def make_verb(match_object):
	return [('verb', match_object.group('code'))]

def do_include_file(m):
	"m: MatchObject"
	return [('input', get_output ('pagebreak'))] \
	     + read_doc_file(m.group('filename')) \
	     + [('input', get_output ('pagebreak'))] 

def do_input_file(m):
	return read_doc_file(m.group('filename'))

def make_lilypond(m):
	if m.group('options'):
		options = m.group('options')
	else:
		options = ''
	return [('input', get_output('output-lilypond-fragment') % 
			(options, m.group('code')))]

def make_lilypond_file(m):
	'''

	Find @lilypondfile{bla.ly} occurences and substitute bla.ly
	into a @lilypond .. @end lilypond block.
	
	'''
	
	if m.group('options'):
		options = m.group('options')
	else:
		options = ''
	(content, nm) = find_file(m.group('filename'))
	options = "filename=%s," % nm + options

	return [('input', get_output('output-lilypond') %
			(options, content))]

def make_lilypond_block(m):
	if not g_do_music:
		return []
	
	if m.group('options'):
		options = get_re('option-sep').split (m.group('options'))
	else:
	    options = []
	options = filter(lambda s: s != '', options)
	return [('lilypond', m.group('code'), options)]

def do_columns(m):
	if __main__.format != 'latex':
		return []
	if m.group('num') == 'one':
		return [('numcols', m.group('code'), 1)]
	if m.group('num') == 'two':
		return [('numcols', m.group('code'), 2)]

def do_multicols(m):
	if __main__.format != 'latex':
		return []
	if m.group('be') == 'begin':
		return [('multicols', m.group('code'), int(m.group('num')))]
	else:
		return [('multicols', m.group('code'), 1)]
	return []

def chop_chunks(chunks, re_name, func, use_match=0):
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
				        #newchunks.extend(func(m))
					# python 1.5 compatible:
					newchunks = newchunks + func(m)
					str = str [m.end(0):]
		else:
			newchunks.append(c)
	return newchunks

def determine_format (str):
	if __main__.format == '':
		
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
		__main__.format = f

	if __main__.paperguru == None:
		if __main__.format == 'html':
			g = HtmlPaper ()
		elif __main__.format == 'latex':
			g = LatexPaper ()
		elif __main__.format == 'texi':
			g = TexiPaper ()
			
		__main__.paperguru = g


def read_doc_file (filename):
	'''Read the input file, find verbatim chunks and do \input and \include
	'''
	(str, path) = find_file(filename)
	determine_format (str)
	
	chunks = [('input', str)]
	
	# we have to check for verbatim before doing include,
	# because we don't want to include files that are mentioned
	# inside a verbatim environment
	chunks = chop_chunks(chunks, 'verbatim', make_verbatim)
	chunks = chop_chunks(chunks, 'verb', make_verb)
	chunks = chop_chunks(chunks, 'multiline-comment', do_ignore)
	#ugh fix input
	chunks = chop_chunks(chunks, 'include', do_include_file, 1)
	chunks = chop_chunks(chunks, 'input', do_input_file, 1)
	return chunks


taken_file_names = {}
def schedule_lilypond_block (chunk):
	'''Take the body and options from CHUNK, figure out how the
	real .ly should look, and what should be left MAIN_STR (meant
	for the main file).  The .ly is written, and scheduled in
	TODO.

	Return: a chunk (TYPE_STR, MAIN_STR, OPTIONS, TODO, BASE)

	TODO has format [basename, extension, extension, ... ]
	
	'''
	(type, body, opts) = chunk
	assert type == 'lilypond'
	file_body = compose_full_body (body, opts)
	## Hmm, we should hash only lilypond source, and skip the
	## %options are ...
	## comment line
	basename = 'lily-' + `abs(hash (file_body))`
	for o in opts:
		m = re.search ('filename="(.*?)"', o)
		if m:
			basename = m.group (1)
			if not taken_file_names.has_key(basename):
				taken_file_names[basename] = 0
			else:
				taken_file_names[basename] = taken_file_names[basename] + 1
				basename = basename + "-%i" % taken_file_names[basename]
	if not g_read_lys:
		update_file(file_body, os.path.join(g_outdir, basename) + '.ly')
	needed_filetypes = ['tex']

	if format == 'html' or format == 'texi':
		needed_filetypes.append ('eps')
		needed_filetypes.append ('png')
	if 'eps' in opts and not ('eps' in needed_filetypes):
		needed_filetypes.append('eps')
	pathbase = os.path.join (g_outdir, basename)
	def f (base, ext1, ext2):
		a = os.path.isfile(base + ext2)
		if (os.path.isfile(base + ext1) and
		    os.path.isfile(base + ext2) and
				os.stat(base+ext1)[stat.ST_MTIME] >
				os.stat(base+ext2)[stat.ST_MTIME]) or \
				not os.path.isfile(base + ext2):
			return 1
	todo = []
	if 'tex' in needed_filetypes and f(pathbase, '.ly', '.tex'):
		todo.append('tex')
	if 'eps' in needed_filetypes and f(pathbase, '.tex', '.eps'):
		todo.append('eps')
	if 'png' in needed_filetypes and f(pathbase, '.eps', '.png'):
		todo.append('png')
	newbody = ''

	if 'printfilename' in opts:
		for o in opts:
			m= re.match ("filename=(.*)", o)
			if m:
				newbody = newbody + get_output ("output-filename") % m.group(1)
				break
		

	if 'smallverbatim' in opts:
		newbody = output_verbatim (body, 1)
	elif 'verbatim' in opts:
		newbody = output_verbatim (body, 0)

	for o in opts:
		m = re.search ('intertext="(.*?)"', o)
		if m:
			newbody = newbody  + m.group (1) + "\n\n"
	
	if 'noinline' in opts:
		s = 'output-noinline'
	elif format == 'latex':
		if 'eps' in opts:
			s = 'output-eps'
		else:
			s = 'output-tex'
	else: # format == 'html' or format == 'texi':
		s = 'output-all'
	newbody = newbody + get_output (s) % {'fn': basename }
	return ('lilypond', newbody, opts, todo, basename)

def process_lilypond_blocks(chunks):#ugh rename
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



def system (cmd):
	sys.stderr.write ("invoking `%s'\n" % cmd)
	st = os.system (cmd)
	if st:
		error ('Error command exited with value %d\n' % st)
	return st

def quiet_system (cmd, name):
	if not verbose_p:
		progress ( _("Running %s...") % name)
		cmd = cmd + ' 1> /dev/null 2> /dev/null'

	return system (cmd)

def get_bbox (filename):
	system ('gs -sDEVICE=bbox -q  -sOutputFile=- -dNOPAUSE %s -c quit > %s.bbox 2>&1 ' % (filename, filename))

	box = open (filename + '.bbox').read()
	m = re.match ('^%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)', box)
	gr = []
	if m:
		gr = map (string.atoi, m.groups ())
	
	return gr

def make_pixmap (name):
	bbox = get_bbox (name + '.eps')
	margin = 0
	fo = open (name + '.trans.eps' , 'w')
	fo.write ('%d %d translate\n' % (-bbox[0]+margin, -bbox[1]+margin))
	fo.close ()
	
	res = 90

	x = (2* margin + bbox[2] - bbox[0]) * res / 72.
	y = (2* margin + bbox[3] - bbox[1]) * res / 72.

	cmd = r'''gs -g%dx%d -sDEVICE=pnggray  -dTextAlphaBits=4 -dGraphicsAlphaBits=4  -q -sOutputFile=- -r%d -dNOPAUSE %s %s -c quit  > %s'''
	
	cmd = cmd % (x, y, res, name + '.trans.eps', name + '.eps',name + '.png')
	status = 0
	try:
		status = system (cmd)
	except:
		status = -1

	if status:
		os.unlink (name + '.png')
		error ("Removing output file")

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
	d = os.getcwd()
	if g_outdir:
		os.chdir(g_outdir)
	if tex:
		# fixme: be sys-independent.
		def incl_opt (x):
			if g_outdir and x[0] != '/' :
				x = os.path.join (g_here_dir, x)
			return ' -I %s' % x

		incs = map (incl_opt, include_path)
		lilyopts = string.join (incs, ' ' )
		if do_deps:
			lilyopts = lilyopts + ' --dependencies '
			if g_outdir:
				lilyopts = lilyopts + '--dep-prefix=' + g_outdir + '/'
		texfiles = string.join (tex, ' ')
		cmd = 'lilypond --header=texidoc %s %s %s' \
		      % (lilyopts, g_extra_opts, texfiles)

		system (cmd)

		#
		# Ugh, fixing up dependencies for .tex generation
		#
		if do_deps:
			depfiles=map (lambda x: re.sub ('(.*)\.ly', '\\1.dep', x), tex)
			for i in depfiles:
				f =open (i)
				text=f.read ()
				f.close ()
				text=re.sub ('\n([^:\n]*):', '\n' + foutn + ':', text)
				f = open (i, 'w')
				f.write (text)
				f.close ()

	for e in eps:
		cmd = r"echo $TEXMF; tex '\nonstopmode \input %s'" % e
		quiet_system (cmd, 'TeX')
		
		cmd = r"dvips -E -o %s %s" % (e + '.eps', e)
		quiet_system (cmd, 'dvips')
		
	for g in png:
		make_pixmap (g)
		
	os.chdir (d)


def update_file (body, name):
	'''
	write the body if it has changed
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
	sys.stdout.write('''Usage: lilypond-book [options] FILE\n
Generate hybrid LaTeX input from Latex + lilypond
Options:
''')
	sys.stdout.write (options_help_str (option_definitions))
	sys.stdout.write (r'''Warning all output is written in the CURRENT directory



Report bugs to bug-lilypond@gnu.org.

Written by Tom Cato Amundsen <tca@gnu.org> and
Han-Wen Nienhuys <hanwen@cs.uu.nl>
''')

	sys.exit (0)


def write_deps (fn, target, chunks):
	global read_files
	sys.stderr.write('Writing `%s\'\n' % os.path.join(g_outdir, fn))
	f = open (os.path.join(g_outdir, fn), 'w')
	f.write ('%s%s: ' % (g_dep_prefix, target))
	for d in read_files:
		f.write ('%s ' %  d)
	basenames=[]
        for c in chunks:
	        if c[0] == 'lilypond':
			(type, body, opts, todo, basename) = c;
			basenames.append (basename)
	for d in basenames:
		if g_outdir:
			d=g_outdir + '/' + d
		if g_dep_prefix:
			#if not os.isfile (d): # thinko?
			if not re.search ('/', d):
				d = g_dep_prefix + d
		f.write ('%s.tex ' %  d)
	f.write ('\n')
	#if len (basenames):
	#	for d in basenames:
	#		f.write ('%s.ly ' %  d)
	#	f.write (' : %s' % target)
	f.write ('\n')
	f.close ()
	read_files = []

def identify (stream):
	stream.write ('lilypond-book (GNU LilyPond) %s\n' % program_version)

def print_version ():
	identify (sys.stdout)
	sys.stdout.write (r'''Copyright 1998--1999
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.
''')


def check_texidoc (chunks):
	n = []
        for c in chunks:
	        if c[0] == 'lilypond':
			(type, body, opts, todo, basename) = c;
			pathbase = os.path.join (g_outdir, basename)
			if os.path.isfile (pathbase + '.texidoc'):
				body = '\n@include %s.texidoc\n' % basename + body
				c = (type, body, opts, todo, basename)
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
		newchunks.append(('lilypond', body, c[2], c[3], c[4]))
			
	return newchunks


##docme: why global?
foutn=""
def do_file(input_filename):

	chunks = read_doc_file(input_filename)
	chunks = chop_chunks(chunks, 'lilypond', make_lilypond, 1)
	chunks = chop_chunks(chunks, 'lilypond-file', make_lilypond_file, 1)
	chunks = chop_chunks(chunks, 'lilypond-block', make_lilypond_block, 1)
	chunks = chop_chunks(chunks, 'singleline-comment', do_ignore, 1)
	chunks = chop_chunks(chunks, 'preamble-end', do_preamble_end)
	chunks = chop_chunks(chunks, 'numcols', do_columns)
	chunks = chop_chunks(chunks, 'multicols', do_multicols)
	#print "-" * 50
	#for c in chunks: print "c:", c;
	#sys.exit()
	scan_preamble(chunks)
	chunks = process_lilypond_blocks(chunks)

	# Do It.
	if __main__.g_run_lilypond:
		compile_all_files (chunks)
		chunks = fix_epswidth (chunks)

	if __main__.format == 'texi':
		chunks = check_texidoc (chunks)

	x = 0
	chunks = completize_preamble (chunks)


	global foutn

	if outname:
		my_outname = outname
	elif input_filename == '-' or input_filename == "/dev/stdin":
		my_outname = '-'
	else:
		my_outname = os.path.basename (os.path.splitext(input_filename)[0]) + '.' + format
	my_depname = my_outname + '.dep'		
	
	if my_outname == '-' or my_outname == '/dev/stdout':
		fout = sys.stdout
		foutn = "<stdout>"
		__main__.do_deps = 0
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
	(sh, long) = getopt_args (__main__.option_definitions)
	(options, files) = getopt.getopt(sys.argv[1:], sh, long)
except getopt.error, msg:
	sys.stderr.write("error: %s" % msg)
	sys.exit(1)

do_deps = 0
for opt in options:	
	o = opt[0]
	a = opt[1]

	if o == '--include' or o == '-I':
		include_path.append (a)
	elif o == '--version' or o == '-v':
		print_version ()
		sys.exit  (0)
	elif o == '--verbose' or o == '-V':
		__main__.verbose_p = 1
	elif o == '--format' or o == '-f':
		__main__.format = a
	elif o == '--outname' or o == '-o':
		if len(files) > 1:
			#HACK
			sys.stderr.write("Lilypond-book is confused by --outname on multiple files")
			sys.exit(1)
		outname = a
	elif o == '--help' or o == '-h':
		help ()
	elif o == '--no-lily' or o == '-n':
		__main__.g_run_lilypond = 0
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
		g_force_music_fontsize = string.atoi(a)
	elif o == '--force-lilypond-fontsize':
		print "--force-lilypond-fontsize is deprecated, use --default-lilypond-fontsize"
		g_force_music_fontsize = string.atoi(a)
	elif o == '--dep-prefix':
		g_dep_prefix = a
	elif o == '--no-pictures':
		g_do_pictures = 0
	elif o == '--no-music':
		g_do_music = 0
	elif o == '--read-lys':
		g_read_lys = 1
	elif o == '--outdir':
		g_outdir = a

identify (sys.stderr)
if g_outdir:
	if os.path.isfile(g_outdir):
		error ("outdir is a file: %s" % g_outdir)
	if not os.path.exists(g_outdir):
		os.mkdir(g_outdir)
setup_environment ()
for input_filename in files:
	do_file(input_filename)
	
#
# Petr, ik zou willen dat ik iets zinvoller deed,
# maar wat ik kan ik doen, het verandert toch niets?
#   --hwn 20/aug/99
