#!@PYTHON@

'''
TODO: \input, @include
      check latex parameters
      papersizes?
      


Example usage:

test:
     filter-lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK
	  
convert-ly on book:
     filter-lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

minimal classic lilypond-book (WIP):
     filter-lilypond-book --process="lilypond-bin" BOOK.tely

     (([0-9][0-9])*pt) -> staffsize=\2
     
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
process_cmd = 'lilypond-bin'
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
re_dict = {
	HTML: {
		'include':  no_match,
		'input': no_match,
		'header': no_match,
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

	LATEX: {
		'junkmeinput': r'(?m)^[^%\n]*?(?P<match>\\mbinput{?([^}\t \n}]*))',
		'junkmeinclude': r'(?m)^[^%\n]*?(?P<match>\\mbinclude{(?P<filename>[^}]+)})',
		'include': r'(?m)^[^%\n]*?(?P<match>\\input{(?P<filename>[^}]+)})',
		'option-sep' : ',\s*',
		'header': r"\n*\\documentclass\s*(\[.*?\])?",
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

	TEXINFO: {
		'junkmeinclude':  '(?m)^[^%\n]*?(?P<match>@mbinclude\s+(?P<filename>\S*))',
		'include':  '(?m)^[^%\n]*?(?P<match>@include\s+(?P<filename>\S*))',
		'input': no_match,
		'header': no_match,
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

# WIP
output = {
	HTML : {
	AFTER: '',
	BEFORE: '',
	OUTPUT: r'''<src image="%(base)s.png">''',
	VERBATIM: r'''<pre>
%(code)s</pre>''',
	},
	
	LATEX :	{
	AFTER: '',
	BEFORE: '',
	OUTPUT: r'''{\parindent 0pt
\ifx\preLilyPondExample\preLilyPondExample\fi
\def\lilypondbook{}
\input %(base)s.tex
\ifx\preLilyPondExample\postLilyPondExample\fi}
''',
	VERBATIM: r'''\begin{verbatim}
%(code)s\end{verbatim}
''',
	},
	
	TEXINFO :	{
	BEFORE: '',
	AFTER: '',
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
		options = options + string.split (option_string, ',')
	
	m = re.search (r'''\\score''', code)
	if not m and (not options \
		      or not 'nofragment' in options \
		      or 'fragment' in options):
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

## make source, index statics of Snippet?
index = 0

def find_file (name):
	for i in include_path:
		full = os.path.join (i, name)
		if os.path.exists (full):
			return full
	ly.error (_ ('file not found: %s\n' % name))
	ly.exit (1)
	return ''
	
class Snippet:
	def __init__ (self, type, index, match):
		self.type = type
		self.index = index
		self.match = match
		self.hash = 0

	def start (self, s):
		return self.index + self.match.start (s)

	def end (self, s):
		return self.index + self.match.end (s)

	def substring (self, source, s):
		return source[self.start (s):self.end (s)]

	def ly (self, source):
		s = ''
		if self.type == 'lilypond-block' or self.type == 'lilypond':
			s = self.substring (source, 'code')
		elif self.type == 'lilypond-file':
			name = self.substring (source, 'filename')
			s = open (find_file (name)).read ()
		return s
		
	def full_ly (self, source):
		s = self.ly (source)
		if s:
			return compose_ly (s, self.match.group ('options'))
		return ''
	
	def get_hash (self, source):
		if not self.hash:
			self.hash = abs (hash (self.ly (source)))
		if not self.hash:
			print 'TYPE:: ' + self.type
			print 'CODE:: ' + self.substring (source, 0)
			raise 'URG'
		return self.hash

	def basename (self, source):
		if use_hash_p:
			return 'lily-%d' % self.get_hash (source)
		raise 'to be done'

	def write_ly (self, source):
		if self.type == 'lilypond-block' or self.type == 'lilypond'\
		       or self.type == 'lilypond-file':
			h = open (self.basename (source) + '.ly', 'w')
			h.write (self.full_ly (source))
			h.close ()

	def output_html (self, source):
		base = self.basename (source)
		option_string = self.match.group ('options')
		if option_string and VERBATIM in string.split (option_string):
			code = self.substring (source, 'code')
			h.write (output[HTML][VERBATIM] % vars ())
		h.write (output[HTML][BEFORE])
		h.write (output[HTML][OUTPUT] % vars ())
		h.write (output[HTML][AFTER])
			
	def output_latex (self, source):
		option_string = self.match.group ('options')
		if option_string and VERBATIM in string.split (option_string):
			code = self.substring (source, 'code')
			h.write (output[LATEX][VERBATIM] % vars ())
		h.write (output[LATEX][BEFORE])
		base = self.basename (source)
		h.write (output[LATEX][OUTPUT] % vars ())
		h.write (output[LATEX][AFTER])
			
	def output_texinfo (self, source):
		h.write ('\n@tex\n')
		self.output_latex (source)
		h.write ('\n@end tex\n')
		
		h.write ('\n@html\n')
		self.output_html (source)
		h.write ('\n@end html\n')
			
	def outdated_p (self, source):
		if self.type != 'lilypond-block' and self.type != 'lilypond'\
		       and self.type != 'lilypond-file':
			return None
		base = self.basename (source)
		if os.path.exists (base + '.ly') \
		   and os.path.exists (base + '.tex') \
		   and (use_hash_p \
			or self.ly (source) == open (base + '.ly').read ()):
			# TODO: something smart with target formats
			# (ps, png) and m/ctimes
			return None
		return self

def find_snippets (s, type):
	re = ly.re.compile (re_dict[format][type])
	i = 0
	snippets = []
	m = re.search (s[i:])
	while m:
		snippets.append (Snippet (type, i, m))
		i = i + m.end (0)
		m = re.search (s[i:])
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

def compare_index (a, b):
	return a.start (0) - b.start (0)

# apply FUNC to every toplevel block in SNIPPETS, ie, enclosed
# snippets are skipped.  return list with all non-empty return values
# of FUNC

# Hmm, do we need enclosed snippets at all?  Maybe use MAP_SNIPPETS
# once and use simple filter/map on that resulting toplevel list iso
# silly map_snippets/do_snippets.
def map_snippets (source, snippets, func):
	global index
	index = 0
	lst = []
 	for i in snippets:
 		if i.start (0) < index:
 			continue
		# lst.append (func (i, source))
		x = func (i, source)
		if x:
			lst.append (x)
 		index = i.end (0)
	return lst

# apply FUNC to every toplevel block in SNIPPETS, ie, enclosed
# snippets are skipped.  return last snippet's index
def do_snippets (source, snippets, func):
	global index
	index = 0
 	for i in snippets:
 		if i.start (0) < index:
 			continue
		func (i, source)
		# ugr, moved to FUNC
 		#index = i.end ('code')
	return index

def process_snippets (source, snippets, cmd):
	names = map_snippets (source, snippets, Snippet.basename)
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


def do_file (input_filename):
	global format
	
	if not format:
		ext2format = {
			'.html' : HTML,
			'.itely' : TEXINFO,
			'.lytex' : LATEX,
			'.tely' : TEXINFO,
			'.tex': LATEX,
			'.texi' : TEXINFO,
			'.xml' : HTML,
			}
			       
		e = os.path.splitext (input_filename)[1]
		if e in ext2format.keys ():
			format = ext2format[e]
		else:
			ly.error (_ ("cannot determine format for: %s" \
				     % input_filename))

	global h

	h = sys.stdin
	if input_filename != '-':
		h = open (input_filename)
	source = h.read ()

	snippet_types = ('verbatim', 'verb', 'multiline-comment',
			 'lilypond', 'lilypond-block', 'lilypond-file',)
	snippets = []
	for i in snippet_types:
		snippets += find_snippets (source, i)

	snippets.sort (compare_index)

	h = sys.stdout
	if output_name and output_name != '-':
		if not os.path.isdir (output_name):
			os.mkdir (output_name, 0777)
		if input_filename != '-':
			h = open (output_name + '/' \
				  + os.path.split (input_filename)[0] \
				  + '.' + format,
				  'w')
		os.chdir (output_name)

	def filter_source (snippet, source):
		global index
		# Hmm, why is verbatim's group called 'code'; rename to 'verb'?
		#if snippet.match.group ('code'):
		# urg
		if snippet.type == 'lilypond' or snippet.type == 'lilypond-block':
			h.write (source[index:snippet.start ('code')])
			h.write (run_filter (snippet.substring (source, 'code')))
			h.write (source[snippet.end ('code'):snippet.end (0)])
		else:
			h.write (source[index:snippet.end (0)])
		index = snippet.end (0)

	snippet_output = eval ("Snippet.output_" + format)
	def compile_output (snippet, source):
		global index
		# Hmm, why is verbatim's group called 'code'; rename to 'verb'?
		# if snippet.match.group ('code'):
		# urg
		if snippet.type == 'lilypond' \
		       or snippet.type == 'lilypond-block'\
		       or snippet.type == 'lilypond-file':
			h.write (source[index:snippet.start (0)])
			snippet_output (snippet, source)
 		index = snippet.end (0)

	global default_ly_options
	if format == LATEX and LINEWIDTH not in default_ly_options.keys ():
		textwidth = get_latex_textwidth (source)
		default_ly_options[LINEWIDTH] = '''%.0f\pt''' % textwidth

	global index
	if filter_cmd:
		index = do_snippets (source, snippets, filter_source)
		h.write (source[index:])
	elif process_cmd:
		outdated = map_snippets (source, snippets, Snippet.outdated_p)
		do_snippets (source, snippets, Snippet.write_ly)
		process_snippets (source, outdated, process_cmd)
		do_snippets (source, snippets, compile_output)
		h.write (source[index:])

	# misusing snippet bit silly?
	includes = find_snippets (source, 'include')
	for i in includes:
		os.chdir (original_dir)
		sys.stderr.write ('DO: ' + sys.argv[0] + ' OPTIONS ' + i.substring (source, 'filename') + '\n')

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
