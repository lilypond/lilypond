#!@PYTHON@

'''
TODO:
      ly-options: intertext, quote ?
      --linewidth
      check latex parameters, twocolumn
      multicolumn?
      papersizes?
      ly2dvi/notexidoc?
      
Example usage:

test:
     filter-lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK
	  
convert-ly on book:
     filter-lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

classic lilypond-book (WIP):
     filter-lilypond-book --process="lilypond-bin" BOOK.tely

  options in .tely/.lytex must be converted (or grokked by me :-)
     (([0-9][0-9])*pt) -> staffsize=\2
     @mbinclude foo.itely -> @include foo.itely
     \mbinput -> \input
     relative X -> relative=X
     singleline -> raggedright
     
'''

import string
import __main__

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
#program_name = 'new-book'
program_name = 'filter-lilypond-book'
verbose_p = 0
pseudo_filter_p = 0
original_dir = os.getcwd ()


# help_summary = _ ("Process LilyPond snippets in hybrid html, LaTeX or texinfo document")
help_summary = _ ("""Process ly snippets from lilypond-book source.  Example usage:

   filter-lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK
   filter-lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

""")
copyright = ('Jan Nieuwenhuizen <janneke@gnu.org>>',
	     'Han-Wen Nienhuys <hanwen@cs.uu.nl>')

option_definitions = [
	(_ ("EXT"), 'f', 'format', _ ("use output format EXT (texi [default], texi-html, latex, html)")),
	(_ ("FILTER"), 'F', 'filter', _ ("pipe snippets through FILTER [convert-ly -n -]")),
	('', 'h', 'help', _ ("print this help")),
	(_ ("DIR"), 'I', 'include', _ ("add DIR to include path")),
	(_ ("COMMAND"), 'P', 'process', _ ("process ly_files using COMMAND FILE...")),
	(_ ("FILE"), 'o', 'output', _ ("write output to FILE")),
	('', 'V', 'verbose', _ ("be verbose")),
	('', 'v', 'version', _ ("print version information")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	('', 'M', 'dependencies', _ ("TODO")),
	(_ ("DIR"), '', 'outdir', _ ("DEPRECATED, use --output")),
	]

include_path = [ly.abspath (os.getcwd ())]

lilypond_binary = os.path.join ('@bindir@', 'lilypond-bin')

# only use installed binary  when we're installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
	lilypond_binary = 'lilypond-bin'


use_hash_p = 1
format = 0
output_name = 0
latex_filter_cmd = 'latex "\\nonstopmode \input /dev/stdin"'
##filter_cmd = 'convert-ly --no-version --from=2.0.0 -'
filter_cmd = 0

#process_cmd = 'convert-ly --no-version --from=2.0.0'
#process_cmd = 0
# process_cmd = 'lilypond-bin'
process_cmd = 'lilypond-bin -I ../../../input/test'
default_ly_options = { }

LATEX = 'latex'
HTML = 'html'
TEXINFO = 'texinfo'
BEFORE = 'before'
OUTPUT = 'output'
AFTER = 'after'
VERBATIM = 'verbatim'
LINEWIDTH = 'linewidth'

## lilypond-book heritage.  to be cleaned

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
no_match = 'a\ba'
snippet_res = {
	HTML: {
	#'header': no_match,
	#'input': no_match,
	#'landscape': no_match,
	#'ly2dvi': r'(?m)(?P<match><ly2dvifile(?P<options>[^>]+)?>\s*(?P<filename>[^<]+)\s*</ly2dvifile>)',
	#'multicols': no_match,
	#'numcols': no_match,
	'include':  no_match,
	'lilypond' : '(?m)(?P<match><lilypond((?P<options>[^:]*):)(?P<code>.*?)/>)',
	'lilypond-block': r'''(?ms)(?P<match><lilypond(?P<options>[^>]+)?>(?P<code>.*?)</lilypond>)''',
	'lilypond-file': r'(?m)(?P<match><lilypondfile(?P<options>[^>]+)?>\s*(?P<filename>[^<]+)\s*</lilypondfile>)',
	'multiline-comment': r"(?sm)\s*(?!@c\s+)(?P<code><!--\s.*?!-->)\s",
	'singleline-comment': no_match,
	'verb': r'''(?P<code><pre>.*?</pre>)''',
	'verbatim': r'''(?s)(?P<code><pre>\s.*?</pre>\s)''',
	},

	LATEX: {
	#'def-post-re': r"\\def\\postLilyPondExample",
	#'def-pre-re': r"\\def\\preLilyPondExample",
	#'header': r"\n*\\documentclass\s*(\[.*?\])?",
	#'ly2dvi': no_match,
	#'multicols': r"(?P<code>\\(?P<be>begin|end)\s*{multicols}({(?P<num>\d+)?})?)",
	#'numcols': r"(?P<code>\\(?P<num>one|two)column)",
	#'usepackage-graphics': r"\usepackage\s*{graphics}",
	'include': r'(?m)^[^%\n]*?(?P<match>\\input{(?P<filename>[^}]+)})',
	'lilypond' : r'(?m)^[^%\n]*?(?P<match>\\lilypond\s*(\[(?P<options>.*?)\])?\s*{(?P<code>.*?)})',
	'lilypond-block': r"(?sm)^[^%\n]*?(?P<match>\\begin\s*(\[(?P<options>.*?)\])?\s*{lilypond}(?P<code>.*?)\\end{lilypond})",
	'lilypond-file': r'(?m)^[^%\n]*?(?P<match>\\lilypondfile\s*(\[(?P<options>.*?)\])?\s*\{(?P<filename>.+)})',
	'multiline-comment': no_match,
	'singleline-comment': r"(?m)^.*?(?P<match>(?P<code>^%.*$\n+))",
	'verb': r"(?P<code>\\verb(?P<del>.).*?(?P=del))",
	'verbatim': r"(?s)(?P<code>\\begin\s*{verbatim}.*?\\end{verbatim})",
	},

	TEXINFO: {
	#'header': no_match,
	#'landscape': no_match,
	#'multicols': no_match,
	#'numcols': no_match,
	'include':  '(?m)^[^%\n]*?(?P<match>@include\s+(?P<filename>\S*))',
	'lilypond' : '(?m)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?{(?P<code>.*?)})',
	'lilypond-block': r'''(?ms)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?\s(?P<code>.*?)@end lilypond)\s''',
	'lilypond-file': '(?m)^(?P<match>@lilypondfile(\[(?P<options>[^]]*)\])?{(?P<filename>[^}]+)})',
	'multiline-comment': r"(?sm)^\s*(?!@c\s+)(?P<code>@ignore\s.*?@end ignore)\s",
	'singleline-comment': r"(?m)^.*?(?P<match>(?P<code>@c.*$\n+))",
	'verb': r'''(?P<code>@code{.*?})''',
	'verbatim': r'''(?s)(?P<code>@example\s.*?@end example\s)''',
	},
	}

format_res = {
	HTML: {
	'option-sep' : '\s*',
	'intertext': r',?\s*intertext=\".*?\"',
	},
	LATEX: {
	'intertext': r',?\s*intertext=\".*?\"',
	'option-sep' : ',\s*',
	},
	TEXINFO: {
	'intertext': r',?\s*intertext=\".*?\"',
	'option-sep' : ',\s*',
	},
	}

NOTES = 'body'
PREAMBLE = 'preamble'
PAPER = 'paper'

ly_options = {
	NOTES: {
	#'xrelative': r'''\relative #(ly:make-pitch %(relative)s 0 0)''',
	'relative': r'''\relative c%(relative_quotes)s''',
	},
	PAPER: {
	'indent' : r'''
    indent = %(indent)s''',
	'linewidth' : r'''
    linewidth = %(linewidth)s''',
	'noindent' : r'''
    indent = 0.0\mm''',
	'notime' : r'''
    \translator {
        \StaffContext
        \remove Time_signature_engraver
    }''',
	'raggedright' : r'''
    indent = 0.0\mm
    raggedright = ##t''',
	},
	PREAMBLE: {
	'staffsize': r'''
#(set-global-staff-size %(staffsize)s)''',
	},
	}

output = {
	HTML : {
	AFTER: '',
	BEFORE: '',
	OUTPUT: r'''<img align="center" valign="center"
border="0" src="%(base)s.png" alt="[picture of music]">''',
	VERBATIM: r'''<pre>
%(verb)s</pre>''',
	},
	
	LATEX :	{
	AFTER: '',
	BEFORE: '',
	OUTPUT: r'''{\parindent 0pt
\catcode`\@=12
\ifx\preLilyPondExample\preLilyPondExample\fi
\def\lilypondbook{}
\input %(base)s.tex
\ifx\preLilyPondExample\postLilyPondExample\fi
\catcode`\@=0}''',
	VERBATIM: r'''\begin{verbatim}
%(verb)s\end{verbatim}
''',
	},
	
	TEXINFO :	{
	BEFORE: '',
	AFTER: '',
	VERBATIM: r'''@example
%(verb)s@end example''',
	
	},
	
	}

PREAMBLE_LY = r'''%% Generated by %(program_name)s
%% Options: [%(option_string)s]
%(preamble_string)s
\paper {%(paper_string)s
}
''' 

FRAGMENT_LY = r'''\score{
    \notes%(notes_string)s{
        %(code)s    }
}'''
FULL_LY = '%(code)s'


def compose_ly (code, option_string):
	options = []
	# urg
	for i in default_ly_options.keys ():
		options.append (i)
		vars ()[i] = default_ly_options[i]

	if option_string:
		options = options + split_options (option_string)
	
	m = re.search (r'''\\score''', code)
	if not m and (not options \
		      or not 'nofragment' in options \
		      or 'fragment' in options):
		options.append ('raggedright')
		body = FRAGMENT_LY
	else:
		body = FULL_LY

	# defaults
	relative = 0
	staffsize = '16'

	notes_options = []
	paper_options = []
	preamble_options = []
	for i in options:
		if string.find (i, '=') > 0:
			key, value = string.split (i, '=')
			# hmm
			vars ()[key] = value
		else:
			key = i

		if key in ly_options[NOTES].keys ():
			notes_options.append (ly_options[NOTES][key])
		elif key in ly_options[PREAMBLE].keys ():
			preamble_options.append (ly_options[PREAMBLE][key])
		elif key in ly_options[PAPER].keys ():
			paper_options.append (ly_options[PAPER][key])
		elif key not in ('fragment', 'nofragment',
				 'relative', 'verbatim'):
			ly.warning (_("ignoring unknown ly option: %s") % i)

	relative_quotes = (",,,", ",,", ",", "", "'", "''", "'''")[relative-3]
	program_name = __main__.program_name
	notes_string = string.join (notes_options, '\n    ') % vars ()
	paper_string = string.join (paper_options, '\n    ') % vars ()
	preamble_string = string.join (preamble_options, '\n    ') % vars ()
	return (PREAMBLE_LY + body) % vars ()


# BARF
# use lilypond-bin for latex (.lytex) books,
# and lilypond --preview for html, texinfo books?
def to_eps (file):
	cmd = r'latex "\nonstopmode \input %s"' % file
	# Ugh.  (La)TeX writes progress and error messages on stdout
	# Redirect to stderr
	cmd = '(( %s  >&2 ) >&- )' % cmd
	ly.system (cmd)
	ly.system ('dvips -Ppdf -u+lilypond.map -E -o %s.eps %s' \
		   % (file, file))

def find_file (name):
	for i in include_path:
		full = os.path.join (i, name)
		if os.path.exists (full):
			return full
	ly.error (_ ('file not found: %s\n' % name))
	ly.exit (1)
	return ''
	
def verbatim_html (s):
	return re.sub ('>', '&gt;',
		       re.sub ('<', '&lt;',
			       re.sub ('&', '&amp;', s)))

def verbatim_texinfo (s):
	return re.sub ('{', '@{',
		       re.sub ('}', '@}',
			       re.sub ('@', '@@', s)))

def split_options (option_string):
	return re.split (format_res[format]['option-sep'], option_string)


## make index static of Snippet?
index = 0

class Snippet:
	def __init__ (self, type, source, index, match):
		self.type = type
		self.source = source
		self.index = index
		self.match = match
		self.hash = 0

	def start (self, s):
		return self.index + self.match.start (s)

	def end (self, s):
		return self.index + self.match.end (s)

	def substring (self, s):
		return self.source[self.start (s):self.end (s)]

	def ly (self):
		s = ''
		if self.type == 'lilypond-block' or self.type == 'lilypond':
			s = self.substring ('code')
		elif self.type == 'lilypond-file':
			name = self.substring ('filename')
			s = open (find_file (name)).read ()
		return s
		
	def full_ly (self):
		s = self.ly ()
		if s:
			return compose_ly (s, self.match.group ('options'))
		return ''
	
	def get_hash (self):
		if not self.hash:
			self.hash = abs (hash (self.ly ()))
		if not self.hash:
			print 'TYPE:: ' + self.type
			print 'CODE:: ' + self.substring (0)
			raise 'URG'
		return self.hash

	def basename (self):
		if use_hash_p:
			return 'lily-%d' % self.get_hash ()
		raise 'to be done'

	def write_ly (self):
		if self.type == 'lilypond-block' or self.type == 'lilypond'\
		       or self.type == 'lilypond-file':
			h = open (self.basename () + '.ly', 'w')
			h.write (self.full_ly ())
			h.close ()

	def output_html (self):
		base = self.basename ()
		option_string = self.match.group ('options')
		if option_string and VERBATIM in split_options (option_string)\
		   and format == HTML:
			verb = verbatim_html (self.substring ('code'))
			h.write (output[HTML][VERBATIM] % vars ())
		h.write (output[HTML][BEFORE])
		h.write (output[HTML][OUTPUT] % vars ())
		h.write (output[HTML][AFTER])
			
	def output_latex (self):
		option_string = self.match.group ('options')
		if option_string and VERBATIM in split_options (option_string)\
		   and format == LATEX:
			verb = self.substring ('code')
			h.write (output[LATEX][VERBATIM] % vars ())
		h.write (output[LATEX][BEFORE])
		base = self.basename ()
		h.write (output[LATEX][OUTPUT] % vars ())
		h.write (output[LATEX][AFTER])
			
	def output_texinfo (self):
		option_string = self.match.group ('options')
		if option_string and VERBATIM in split_options (option_string):
			verb = verbatim_texinfo (self.substring ('code'))
			h.write (output[TEXINFO][VERBATIM] % vars ())
		h.write ('\n@tex\n')
		self.output_latex ()
		h.write ('\n@end tex\n')
		
		h.write ('\n@html\n')
		self.output_html ()
		h.write ('\n@end html\n')
			
	def outdated_p (self):
		if self.type != 'lilypond-block' and self.type != 'lilypond'\
		       and self.type != 'lilypond-file':
			return None
		base = self.basename ()
		if os.path.exists (base + '.ly') \
		   and os.path.exists (base + '.tex') \
		   and (use_hash_p \
			or self.ly () == open (base + '.ly').read ()):
			# TODO: something smart with target formats
			# (ps, png) and m/ctimes
			return None
		return self

	def filter_code (self):
		global index
		# Hmm, why is verbatim's group called 'code'; rename to 'verb'?
		#if snippet.match.group ('code'):
		# urg
		if self.type == 'lilypond' or self.type == 'lilypond-block':
			h.write (self.source[index:self.start ('code')])
			h.write (run_filter (self.substring ('code')))
			h.write (self.source[self.end ('code'):self.end (0)])
		else:
			h.write (self.source[index:self.end (0)])
		index = self.end (0)

	def compile_output (self):
		global index
		# Hmm, why is verbatim's group called 'code'; rename to 'verb'?
		# if snippet.match.group ('code'):
		# urg
		if self.type == 'lilypond' \
		       or self.type == 'lilypond-block'\
		       or self.type == 'lilypond-file':
			h.write (self.source[index:self.start (0)])
			snippet_output = eval ("Snippet.output_" + format)
			snippet_output (self)
		elif self.type == 'include':
			h.write (self.source[index:self.start ('filename')])
			base = os.path.splitext (self.substring ('filename'))[0]
			h.write (base + format2ext[format])
			h.write (self.source[self.end ('filename'):self.end (0)])
		else:
			h.write (self.source[index:self.end (0)])
 		index = self.end (0)

def find_toplevel_snippets (s, types):
	res = {}
	for i in types:
		res[i] = ly.re.compile (snippet_res[format][i])

	snippets = []
	index = 0
	while 1:
		matches = {}
		first = 0
		endex = 1 << 30
		for i in types:
			matches[i] = res[i].search (s[index:index+endex])
			if matches[i]:
				start = matches[i].start (0)
				if start < endex:
					endex = start
				if not first \
				       or start < matches[first].start (0):
					first = i
		if not first:
			break
		snippets.append (Snippet (first, s, index, matches[first]))
		index = index + matches[first].end (0)
		
	return snippets

def filter_pipe (input, cmd):
	if verbose_p:
		ly.progress (_ ("Opening filter `%s\'") % cmd)
		
	stdin, stdout, stderr = os.popen3 (cmd)
	stdin.write (input)
	status = stdin.close ()

	if not status:
		status = 0
		output = stdout.read ()
		status = stdout.close ()
		error = stderr.read ()
		
	if not status:
		status = 0
	signal = 0x0f & status
	if status or (not output and error):
		exit_status = status >> 8
		ly.error (_ ("`%s\' failed (%d)") % (cmd, exit_status))
		ly.error (_ ("The error log is as follows:"))
		sys.stderr.write (error)
		sys.stderr.write (stderr.read ())
		ly.exit (status)
	
	if verbose_p:
		ly.progress ('\n')

	return output
	
def run_filter (s):
	return filter_pipe (s, filter_cmd)

def process_snippets (cmd, snippets):
	names = filter (lambda x:x, map (Snippet.basename, snippets))
	if names:
		ly.system (string.join ([cmd] + names))

	if format == HTML or format == TEXINFO:
		for i in names:
			to_eps (i)
			ly.make_ps_images (i + '.eps', resolution=110)

LATEX_DOCUMENT = r'''
%(preamble)s
\begin{document}
\typeout{textwidth=\the\textwidth}
\typeout{columnsep=\the\columnsep}
\makeatletter\if@twocolumn\typeout{columns=2}\fi\makeatother
\end{document}
'''
#need anything else besides textwidth?
def get_latex_textwidth (source):
	m = re.search (r'''(?P<preabmle>\\begin\s*{document})''', source)
	preamble = source[:m.start (0)]
	latex_document = LATEX_DOCUMENT % vars ()
        parameter_string = filter_pipe (latex_document, latex_filter_cmd)

	columns = 0
	m = re.search ('columns=([0-9.]*)', parameter_string)
	if m:
		columns = string.atoi (m.group (1))

	columnsep = 0
	m = re.search ('columnsep=([0-9.]*)pt', parameter_string)
	if m:
		columnsep = string.atof (m.group (1))

	textwidth = 0
	m = re.search('textwidth=([0-9.]*)pt', parameter_string)
	if m:
		textwidth = string.atof (m.group (1))
		if columns:
			textwidth = (textwidth - columnsep) / columns

	return textwidth


ext2format = {
	'.html' : HTML,
	'.itely' : TEXINFO,
	'.lytex' : LATEX,
	'.tely' : TEXINFO,
	'.tex': LATEX,
	'.texi' : TEXINFO,
	'.texinfo' : TEXINFO,
	'.xml' : HTML,
	}
			       
format2ext = {
	HTML: '.html',
	#TEXINFO: '.texinfo',
	TEXINFO: '.texi',
	LATEX: '.tex',
	}

def do_file (input_filename):
	#ugh
	global format
	if not format:
		e = os.path.splitext (input_filename)[1]
		if e in ext2format.keys ():
			#FIXME
			format = ext2format[e]
		else:
			ly.error (_ ("cannot determine format for: %s" \
				     % input_filename))

	ly.progress (_ ("Reading %s...") % input_filename)
	global h
	if not input_filename or input_filename == '-':
		h = sys.stdin
	else:
		h = open (input_filename)
	source = h.read ()
	ly.progress ('\n')

	ly.progress (_ ("Dissecting..."))
	snippets = find_toplevel_snippets (source, snippet_res[format].keys ())
	ly.progress ('\n')

	if output_name == '-' or not output_name:
		h = sys.stdout
		output_filename = '-'
	else:
		if not os.path.isdir (output_name):
			os.mkdir (output_name, 0777)
		if input_filename == '-':
			input_base = 'stdin'
		else:
			input_base = os.path.splitext (input_filename)[0]
		output_filename = output_name + '/' + input_base \
				  + format2ext[format]
		h = open (output_filename, 'w')
		os.chdir (output_name)

	global default_ly_options
	textwidth = 0
	if format == LATEX and LINEWIDTH not in default_ly_options.keys ():
		textwidth = get_latex_textwidth (source)
		default_ly_options[LINEWIDTH] = '''%.0f\pt''' % textwidth

	global index
	if filter_cmd:
		index = 0
		map (Snippet.filter_code, snippets)
		h.write (source[index:])
	elif process_cmd:
		outdated = filter (lambda x:x,
				   map (Snippet.outdated_p, snippets))
		ly.progress (_ ("Writing snippets..."))
		map (Snippet.write_ly, snippets)
		ly.progress ('\n')
		
		if outdated:
			ly.progress (_ ("Processing..."))
			process_snippets (process_cmd, outdated)
		else:
			ly.progress (_ ("All snippets are up to date..."))
		ly.progress ('\n')
		
		ly.progress (_ ("Compiling %s...") % output_filename)
		index = 0
		map (Snippet.compile_output, snippets)
		h.write (source[index:])
		ly.progress ('\n')

	if h != sys.stdout:
		h.close ()

	def process_include (snippet):
		os.chdir (original_dir)
		name = snippet.substring ('filename')
		ly.progress (_ ('Processing include: %s') % name)
		ly.progress ('\n')
		do_file (name)

	map (process_include, filter (lambda x: x.type == 'include', snippets))

def do_options ():
	global format, output_name
	global filter_cmd, process_cmd, verbose_p
	
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
		elif o == '--version' or o == '-v':
			ly.identify (sys.stdout)
			sys.exit (0)
		elif o == '--verbose' or o == '-V':
			verbose_p = 1
		elif o == '--filter' or o == '-F':
			filter_cmd = a
			process_cmd = 0
		elif o == '--format' or o == '-f':
			format = a
			if a == 'texi-html' or a == 'texi':
				format = TEXINFO
		elif o == '--help' or o == '-h':
			ly.help ()
			sys.exit (0)
		elif o == '--include' or o == '-I':
			include_path.append (os.path.join (original_dir,
							   ly.abspath (a)))
		elif o == '--output' or o == '-o':
			output_name = a
		elif o == '--outdir':
			output_name = a
		elif o == '--process' or o == '-P':
			process_cmd = a
			filter_cmd = 0
		elif o == '--warranty' or o == '-w':
			if 1 or status:
				ly.warranty ()
			sys.exit (0)
	return files

def main ():
	files = do_options ()
	ly.identify (sys.stderr)
	ly.setup_environment ()
	if files:
		do_file (files[0])

if __name__ == '__main__':
	main ()
