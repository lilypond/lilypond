#!@PYTHON@

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
copyright = ('Tom Cato Amundsen <tca@gnu.org>',
	     'Han-Wen Nienhuys <hanwen@cs.uu.nl>')

option_definitions = [
	(_ ("EXT"), 'f', 'format', _ ("use output format EXT (texi [default], texi-html, latex, html)")),
	(_ ("FILTER"), 'F', 'filter', _ ("pipe snippets through FILTER [convert-ly -n -]")),
	('', 'h', 'help', _ ("print this help")),
	(_ ("COMMAND"), 'P', 'process', _ ("process ly_files using COMMAND FILE...")),
	('', 'V', 'verbose', _ ("be verbose")),
	('', 'v', 'version', _ ("print version information")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

include_path = [os.getcwd ()]

lilypond_binary = os.path.join ('@bindir@', 'lilypond-bin')

# only use installed binary  when we're installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
	lilypond_binary = 'lilypond-bin'


format = 'latex'
filter_cmd = 'convert-ly --no-version --from=2.0.0 -'
#filter_cmd = 0
#process_cmd = 'convert-ly --no-version --from=2.0.0'
process_cmd = 0

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

def compose_ly (code, options):
	m = re.search (r'''\\score''', code)
	if not m and (not options \
		      or not 'nofragment' in options \
		      or 'fragment' in options):
		body = r'''\score{\notes{ %(code)s }}''' % vars ()
	else:
		body = code
	### todo: add options
	return body

## make source, index statics of Snippet?
index = 0

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
		if self.type == 'lilypond-block' or self.type == 'lilypond':
			return compose_ly (self.substring (source, 'code'),
					   self.match.group ('options'))
		return ''
	
	def get_hash (self, source):
		if not self.hash:
			self.hash = abs (hash (self.substring (source,
							       'code')))
		return self.hash

	def filename (self, source):
		return 'lily-%d.ly' % self.get_hash (source)

	def write_ly (self, source):
		h = open (self.filename (source), 'w')
		h.write (self.ly (source))
		h.close ()

	def outdated_p (self, source):
		if 1:
			return self
		return None

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
	outdated = map_snippets (source, snippets, Snippet.outdated_p)
	names = map_snippets (source, outdated, Snippet.filename)
	if names:
		ly.system (string.join ([cmd] + names))

def do_file (input_filename):
	h = sys.stdin
	if input_filename != '-':
		h = open (input_filename)
	source = h.read ()

	#snippet_types = ('lilypond', 'lilypond-block')
	snippet_types = ('verbatim', 'verb', 'multiline-comment',
			 'lilypond', 'lilypond-block')
	snippets = []
	for i in snippet_types:
		snippets += find_snippets (source, i)

	snippets.sort (compare_index)

	h = sys.stdout
	ext = '.tex'

	def filter_source (snippet, source):
		global index
		# Hmm, why is verbatim's group called 'code'; rename to 'verb'?
		#if snippet.match.group ('code'):
		# urg
		if snippet.type == 'lilypond' or snippet.type == 'lilypond-block':
			h.write (source[index:snippet.start ('code')])
			h.write (run_filter (snippet.substring (source, 'code')))
			#index = snippet.end ('code')
			h.write (source[snippet.end ('code'):snippet.end (0)])
		else:
			h.write (source[index:snippet.end (0)])
		index = snippet.end (0)

	# TODO: output dict?
	def compile_output (snippet, source):
		global index
		# Hmm, why is verbatim's group called 'code'; rename to 'verb'?
		#if snippet.match.group ('code'):
		# urg
		if snippet.type == 'lilypond' or snippet.type == 'lilypond-block':
			h.write (source[index:snippet.start (0)])
			fn = os.path.basename (os.path.splitext (snippet.filename (source))[0]) + ext
			h.write (open (fn).read ())
 		index = snippet.end (0)

	global index
	if filter_cmd:
		index = do_snippets (source, snippets, filter_source)
		h.write (source[index:])
	elif process_cmd:
		do_snippets (source, snippets, Snippet.write_ly)
		process_snippets (source, snippets, process_cmd)
		do_snippets (source, snippets, compile_output)
		h.write (source[index:])

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
		if a == 'texi-html':
			format = 'texi'
	elif o == '--help' or o == '-h':
		ly.help ()
		sys.exit (0)
	elif o == '--process' or o == '-P':
		process_cmd = a
		filter_cmd = 0
	elif o == '--warranty' or o == '-w':
		if 1 or status:
			ly.warranty ()
		sys.exit (0)

ly.identify (sys.stderr)

ly.setup_environment ()

for input_filename in files:
	do_file (input_filename)
