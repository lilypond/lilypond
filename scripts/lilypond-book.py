#!@PYTHON@

'''
TODO:
      ly-options: intertext, quote ?
      --linewidth?
      eps in latex?
      check latex parameters, twocolumn
      multicolumn?
      papersizes?
      ly2dvi/notexidoc?
      
Example usage:

test:
     lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK
	  
convert-ly on book:
     lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

classic lilypond-book:
     lilypond-book --process="lilypond-bin" BOOK.tely

   must substitute:
     @mbinclude foo.itely -> @include foo.itely
     \mbinput -> \input
     
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
program_name = 'lilypond-book'
verbose_p = 0
pseudo_filter_p = 0
original_dir = os.getcwd ()


help_summary = _ ("""Process LilyPond snippets in hybrid HTML, LaTeX or texinfo document.  Example usage:

   lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK
   lilypond-book --filter="convert-ly --no-version --from=2.0.0 -" BOOK
   lilypond-book --process='lilypond-bin -I include' BOOK

""")

copyright = ('Jan Nieuwenhuizen <janneke@gnu.org>>',
	     'Han-Wen Nienhuys <hanwen@cs.uu.nl>')

option_definitions = [
	(_ ("EXT"), 'f', 'format', _ ("use output format EXT (texi [default], texi-html, latex, html)")),
	(_ ("FILTER"), 'F', 'filter', _ ("pipe snippets through FILTER [convert-ly -n -]")),
	('', 'h', 'help', _ ("print this help")),
	(_ ("DIR"), 'I', 'include', _ ("add DIR to include path")),
	(_ ("COMMAND"), 'P', 'process', _ ("process ly_files using COMMAND FILE...")),
	(_ ("DIR"), 'o', 'output', _ ("write output to DIR")),
	('', 'V', 'verbose', _ ("be verbose")),
	('', 'v', 'version', _ ("print version information")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
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
filter_cmd = 0
process_cmd = lilypond_binary
default_ly_options = { }

AFTER = 'after'
BEFORE = 'before'
HTML = 'html'
LATEX = 'latex'
LINEWIDTH = 'linewidth'
NOTES = 'body'
OUTPUT = 'output'
PAPER = 'paper'
PREAMBLE = 'preamble'
TEXINFO = 'texinfo'
VERBATIM = 'verbatim'
PRINTFILENAME = 'printfilename'

# Recognize special sequences in the input 
#
# (?P<name>regex) -- assign result of REGEX to NAME
# *? -- match non-greedily.
# (?m) -- multiline regex: make ^ and $ match at each line
# (?s) -- make the dot match all characters including newline
no_match = 'a\ba'
snippet_res = {
	HTML: {
	'include':  no_match,
	'lilypond' : '(?m)(?P<match><lilypond((?P<options>[^:]*):)(?P<code>.*?)/>)',
	'lilypond_block': r'''(?ms)(?P<match><lilypond(?P<options>[^>]+)?>(?P<code>.*?)</lilypond>)''',
	'lilypond_file': r'(?m)(?P<match><lilypondfile(?P<options>[^>]+)?>\s*(?P<filename>[^<]+)\s*</lilypondfile>)',
	'multiline_comment': r"(?sm)\s*(?!@c\s+)(?P<code><!--\s.*?!-->)\s",
	'singleline_comment': no_match,
	'verb': r'''(?P<code><pre>.*?</pre>)''',
	'verbatim': r'''(?s)(?P<code><pre>\s.*?</pre>\s)''',
	},

	LATEX: {
	'include': r'(?m)^[^%\n]*?(?P<match>\\input{(?P<filename>[^}]+)})',
	'lilypond' : r'(?m)^[^%\n]*?(?P<match>\\lilypond\s*(\[(?P<options>.*?)\])?\s*{(?P<code>.*?)})',
	'lilypond_block': r"(?sm)^[^%\n]*?(?P<match>\\begin\s*(\[(?P<options>.*?)\])?\s*{lilypond}(?P<code>.*?)\\end{lilypond})",
	'lilypond_file': r'(?m)^[^%\n]*?(?P<match>\\lilypondfile\s*(\[(?P<options>.*?)\])?\s*\{(?P<filename>.+)})',
	'multiline_comment': no_match,
	'singleline_comment': r"(?m)^.*?(?P<match>(?P<code>^%.*$\n+))",
	'verb': r"(?P<code>\\verb(?P<del>.).*?(?P=del))",
	'verbatim': r"(?s)(?P<code>\\begin\s*{verbatim}.*?\\end{verbatim})",
	},

	TEXINFO: {
	'include':  '(?m)^[^%\n]*?(?P<match>@include\s+(?P<filename>\S*))',
	'lilypond' : '(?m)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?{(?P<code>.*?)})',
	'lilypond_block': r'''(?ms)^(?P<match>@lilypond(\[(?P<options>[^]]*)\])?\s(?P<code>.*?)@end lilypond)\s''',
	'lilypond_file': '(?m)^(?P<match>@lilypondfile(\[(?P<options>[^]]*)\])?{(?P<filename>[^}]+)})',
	'multiline_comment': r"(?sm)^\s*(?!@c\s+)(?P<code>@ignore\s.*?@end ignore)\s",
	'singleline_comment': r"(?m)^.*?(?P<match>(?P<code>@c([ \t][^\n]*|)\n))",
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

ly_options = {
	NOTES: {
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
	PRINTFILENAME:'<p><tt><a href="%(base)s.ly">%(filename)s</a></tt></p>',
	BEFORE: '',
	OUTPUT: r'''
<img align="center" valign="center"
border="0" src="%(base)s.png" alt="[picture of music]">''',
	VERBATIM: r'''<pre>
%(verb)s</pre>''',
	},
	
	LATEX :	{
	AFTER: '',
	PRINTFILENAME: '''\\texttt{%(filename)s}

	''',
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
%(verb)s@end example
''',
	
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

def classic_lilypond_book_compatibility (o):
	if o == 'singleline':
		return 'raggedright'
	m = re.search ('relative\s*([-0-9])', o)
	if m:
		return 'relative=%s' % m.group (1)
	m = re.match ('([0-9]+)pt', o)
	if m:
		return 'staffsize=%s' % m.group (1)
	m = re.match ('indent=([-.0-9]+)(cm|in|mm|pt)', o)
	if m:
		f = float (m.group (1))
		return 'indent=%f\\%s' % (f, m.group (2))
	m = re.match ('linewidth=([-.0-9]+)(cm|in|mm|pt)', o)
	if m:
		f = float (m.group (1))
		return 'linewidth=%f\\%s' % (f, m.group (2))
	return None

def compose_ly (code, options):
	
	options += default_ly_options.keys ()
	vars ().update (default_ly_options)
	
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
	option_string = string.join (options, ',')
	notes_options = []
	paper_options = []
	preamble_options = []
	for i in options:
		c = classic_lilypond_book_compatibility (i)
		if c:
			ly.warning (_ ("deprecated ly-option used: %s" % i))
			ly.warning (_ ("compatibility mode translation: %s" \
				       % c))
			i = c
		
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
		elif key not in ('fragment', 'nofragment', 'printfilename',
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


class Snippet:
	def __init__ (self, type, match):
		self.type = type
		self.match = match
		self.hash = 0
		self.options = []
		try:
			os = match.group ('options')
			if os:
				self.options = split_options (os)
		except IndexError:
			pass

	def start (self, s):
		return self.match.start (s)

	def end (self, s):
		return self.match.end (s)

	def substring (self, s):
		return self.match.group (s)

	def ly (self):
		s = ''
		if self.type == 'lilypond_block' or self.type == 'lilypond':
			s = self.substring ('code')
		elif self.type == 'lilypond_file':
			name = self.substring ('filename')
			s = open (find_file (name)).read ()
		return s
		
	def full_ly (self):
		s = self.ly ()
		if s:
			return compose_ly (s, self.options)
		return ''
	
	def get_hash (self):
		if not self.hash:
			self.hash = abs (hash (self.ly ()))
		return self.hash

	def basename (self):
		if use_hash_p:
			return 'lily-%d' % self.get_hash ()
		raise 'to be done'

	def write_ly (self):
		if self.type == 'lilypond_block' or self.type == 'lilypond'\
		       or self.type == 'lilypond_file':
			outf = open (self.basename () + '.ly', 'w')
			outf.write (self.full_ly ())

	def replacement_text (self, format):
		if self.type in ['lilypond_file',
				 'lilypond_block',
				 'lilypond']:
			
			func = Snippet.__dict__ ['output_' + format]
			return func (self)
		elif self.type == 'include':
			s = self.match.group (0)
			f = self.substring ('filename')
			nf = os.path.splitext (f)[0] + format2ext[format]
			
			return re.sub (f, nf, s)
		else:
			return self.match.group (0)
	
	def output_html (self):
		base = self.basename ()
		str = self.output_print_filename (HTML)
		if VERBATIM in self.options and format == HTML:
			verb = verbatim_html (self.substring ('code'))
			str  += write (output[HTML][VERBATIM] % vars ())
		return (str + output[HTML][BEFORE] 
			+ (output[HTML][OUTPUT] % vars ())
			+ output[HTML][AFTER])
			
	def output_latex (self):

		str = self.output_print_filename (LATEX)
			
		base = self.basename ()
		str +=  (output[LATEX][BEFORE]
			 + (output[LATEX][OUTPUT] % vars ())
			 + output[LATEX][AFTER])

		
		if  VERBATIM in self.options\
		   and format == LATEX:
			verb = self.substring ('code')
			str += (output[LATEX][VERBATIM] % vars ())

		return str

	def output_print_filename (self,format):
		str = ''
		if  PRINTFILENAME in self.options:
			base = self.basename ()
			filename = self.substring ('filename')
			str += output[format][PRINTFILENAME] % vars ()

		return str
	
	def output_texinfo (self):
		str = ''

		##  Ugh, this breaks texidoc.
		
		str += '\n@tex\n'
		str += self.output_latex ()
		str += ('\n@end tex\n')
		
		str += ('\n@html\n')
		str += self.output_html ()
		str += ('\n@end html\n')

		
		if  VERBATIM in self.options:
			verb = verbatim_texinfo (self.substring ('code'))
			str +=  (output[TEXINFO][VERBATIM] % vars ())
		
		return str
			
	def outdated_p (self):
		if self.type != 'lilypond_block' and self.type != 'lilypond'\
		       and self.type != 'lilypond_file':
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
		pass # todo
	


def simple_find_toplevel_snippets (str, types):
	"return: (new string, snippets)" 
	snippets  = []
	for t in types:
		regex = re.compile (snippet_res[format][t])

		# ugh, this can be done nicer in python 2.x
		def notice_snippet (match, snippets = snippets,
				    t = t, str = str):
			s = Snippet (t, str, match)
			snippets.append (s)

			
			return s.replacement_text (format)

		str = regex.sub (notice_snippet, str)
	return (str, snippets)

def find_toplevel_snippets (infile, outfile, types):
	res = ['(?P<regex%s>%s)' % (t,

				    re.sub (r"\(\?P<[^>]+>", "(", snippet_res[format][t]))
	       for t in types]

	big_re = re.compile (string.join (res, '|'))
	str = infile.read ()
	i = big_re.finditer(str) 

	snips= []
	last_end = 0
	for match in i:
		outfile.write (str[last_end:match.start (0)])
		last_end = match.end (0)
		for t in types:
			m =re.match (snippet_res[format][t], match.group(0))
			if m:
				sn = Snippet (t, m)
				snips.append (sn)
				outfile.write (sn.replacement_text (format))
				break
	return snips		


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
			if os.path.exists (i + '.tex'):
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
	m = re.search (r'''(?P<preamble>\\begin\s*{document})''', source)
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
	if not input_filename or input_filename == '-':
		ih = sys.stdin
	else:
		ih = open (input_filename)

	ly.progress ('\n')

	ly.progress (_ ("Dissecting..."))
	#snippets = find_toplevel_snippets (source, snippet_res[format].keys ())
	snippet_types = (
		'lilypond_block',
		'verb',
		'verbatim',
		'singleline_comment',
		'multiline_comment',
		'lilypond_file',
		'include',
		'lilypond', )
	
	output_file = None
	if output_name == '-' or not output_name:
		output_file = sys.stdout
		output_filename = '-'
	else:
		if not os.path.isdir (output_name):
			os.mkdir (output_name, 0777)
		if input_filename == '-':
			input_base = 'stdin'
		else:
			input_base = os.path.splitext (input_filename)[0]
			input_base = os.path.basename (input_base)
			
		output_filename = output_name + '/' + input_base \
				  + format2ext[format]
		output_file = open (output_filename, 'w')
		os.chdir (output_name)

		
	snippets = find_toplevel_snippets (ih, output_file, snippet_types)
	ly.progress ('\n')


	global default_ly_options
	textwidth = 0
	if format == LATEX and LINEWIDTH not in default_ly_options.keys ():
		textwidth = get_latex_textwidth (source)
		default_ly_options[LINEWIDTH] = '''%.0f\pt''' % textwidth

	if filter_cmd:
		pass # todo
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
		ly.progress ('\n')

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
		elif o == '--version' or o == '-v':
			ly.identify (sys.stdout)
			sys.exit (0)
		elif o == '--verbose' or o == '-V':
			verbose_p = 1
		elif o == '--warranty' or o == '-w':
			if 1 or status:
				ly.warranty ()
			sys.exit (0)
	return files

def main ():

	files = do_options ()
	global process_cmd
	if process_cmd:
		process_cmd += string.join ([(' -I %s' % p)
					     for p in include_path])

	ly.identify (sys.stderr)
	ly.setup_environment ()
	if files:
		do_file (files[0])

if __name__ == '__main__':
	main ()
