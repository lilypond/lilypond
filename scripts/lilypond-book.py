#!@PYTHON@

'''
Example usage:

test:
     lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK

convert-ly on book:
     lilypond-book --filter="convert-ly --no-version --from=1.6.11 -" BOOK

classic lilypond-book:
     lilypond-book --process="lilypond" BOOK.tely

TODO:
    *  ly-options: intertext ?
    *  --linewidth?
    *  eps in latex / eps by lilypond -fps ?
    *  check latex parameters, twocolumn, multicolumn?

    *  Converting from lilypond-book source, substitute:
       @mbinclude foo.itely -> @include foo.itely
       \mbinput -> \input

'''

import __main__
import glob
import stat
import string

#
# TODO:
#
#  * use --png --ps --pdf for making images?
#

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
if os.environ.has_key ('LILYPONDPREFIX'):
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
program_name = sys.argv[0]
verbose_p = 0
pseudo_filter_p = 0
original_dir = os.getcwd ()

help_summary = _ ('''Process LilyPond snippets in hybrid HTML, LaTeX or texinfo document.
Example usage:

   lilypond-book --filter="tr '[a-z]' '[A-Z]'" BOOK
   lilypond-book --filter="convert-ly --no-version --from=2.0.0 -" BOOK
   lilypond-book --process='lilypond -I include' BOOK

''')

copyright = ('Jan Nieuwenhuizen <janneke@gnu.org>',
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
lilypond_binary = os.path.join ('@bindir@', 'lilypond')

# only use installed binary when we're installed too.
if '@bindir@' == ('@' + 'bindir@') or not os.path.exists (lilypond_binary):
	lilypond_binary = 'lilypond'

use_hash_p = 1
format = 0
output_name = 0
latex_filter_cmd = 'latex "\\nonstopmode \input /dev/stdin"'
filter_cmd = 0
process_cmd = ''
default_ly_options = {}

#
# is this pythonic? Personally, I find this rather #define-nesque. --hwn
#
AFTER = 'after'
BEFORE = 'before'
EXAMPLEINDENT = 'exampleindent'
FILTER = 'filter'
FRAGMENT = 'fragment'
HTML = 'html'
INDENT = 'indent'
LATEX = 'latex'
LAYOUT = 'layout'
LINEWIDTH = 'linewidth'
NOFRAGMENT = 'nofragment'
NOINDENT = 'noindent'
NOQUOTE = 'noquote'
NOTES = 'body'
NOTIME = 'notime'
OUTPUT = 'output'
PAPER = 'paper'
PREAMBLE = 'preamble'
PRINTFILENAME = 'printfilename'
QUOTE = 'quote'
RAGGEDRIGHT = 'raggedright'
RELATIVE = 'relative'
STAFFSIZE = 'staffsize'
TEXIDOC = 'texidoc'
TEXINFO = 'texinfo'
VERBATIM = 'verbatim'

# NOTIME has no opposite so it isn't part of this dictionary.
# NOQUOTE is used internally only.
no_options = {
	NOFRAGMENT: FRAGMENT,
	NOINDENT: INDENT,
}

# Recognize special sequences in the input;
#
# (?P<name>regex) -- assign result of REGEX to NAME
# *? -- match non-greedily.
# (?m) -- multiline regex: make ^ and $ match at each line
# (?s) -- make the dot match all characters including newline
# (?x) -- ignore whitespace in patterns
no_match = 'a\ba'
snippet_res = {
	##
	HTML: {
		'include':
		  no_match,

		'lilypond':
		  r'''(?mx)
		    (?P<match>
		    <lilypond
		      (\s*(?P<options>.*?)\s*:)?\s*
		      (?P<code>.*?)
		    />)''',

		'lilypond_block':
		  r'''(?msx)
		    (?P<match>
		    <lilypond
		      \s*(?P<options>.*?)\s*
		    >
		    (?P<code>.*?)
		    </lilypond>)''',

		'lilypond_file':
		  r'''(?mx)
		    (?P<match>
		    <lilypondfile
		      \s*(?P<options>.*?)\s*
		    >
		    \s*(?P<filename>.*?)\s*
		    </lilypondfile>)''',

		'multiline_comment':
		  r'''(?smx)
		    (?P<match>
		    \s*(?!@c\s+)
		    (?P<code><!--\s.*?!-->)
		    \s)''',

		'singleline_comment':
		  no_match,

		'verb':
		  r'''(?x)
		    (?P<match>
		      (?P<code><pre>.*?</pre>))''',

		'verbatim':
		  r'''(?x)
		    (?s)
		    (?P<match>
		      (?P<code><pre>\s.*?</pre>\s))''',
	},

	##
	LATEX: {
		'include':
		  r'''(?smx)
		    ^[^%\n]*?
		    (?P<match>
		    \\input\s*{
		      (?P<filename>\S+?)
		    })''',

		'lilypond':
		  r'''(?smx)
		    ^[^%\n]*?
		    (?P<match>
		    \\lilypond\s*(
		    \[
		      \s*(?P<options>.*?)\s*
		    \])?\s*{
		      (?P<code>.*?)
		    })''',

		'lilypond_block':
		  r'''(?smx)
		    ^[^%\n]*?
		    (?P<match>
		    \\begin\s*(
		    \[
		      \s*(?P<options>.*?)\s*
		    \])?\s*{lilypond}
		      (?P<code>.*?)
		    ^[^%\n]*?
		    \\end\s*{lilypond})''',

		'lilypond_file':
		  r'''(?smx)
		    ^[^%\n]*?
		    (?P<match>
		    \\lilypondfile\s*(
		    \[
		      \s*(?P<options>.*?)\s*
		    \])?\s*\{
		      (?P<filename>\S+?)
		    })''',

		'multiline_comment':
		  no_match,

		'singleline_comment':
		  r'''(?mx)
		    ^.*?
		    (?P<match>
		      (?P<code>
		      %.*$\n+))''',

		'verb':
		  r'''(?mx)
		    ^[^%\n]*?
		    (?P<match>
		      (?P<code>
		      \\verb(?P<del>.)
			.*?
		      (?P=del)))''',

		'verbatim':
		  r'''(?msx)
		    ^[^%\n]*?
		    (?P<match>
		      (?P<code>
		      \\begin\s*{verbatim}
			.*?
		      \\end\s*{verbatim}))''',
	},

	##
	TEXINFO: {
		'include':
		  r'''(?mx)
		    ^(?P<match>
		    @include\s+
		      (?P<filename>\S+))''',

		'lilypond':
		  r'''(?smx)
		    ^[^\n]*?(?!@c\s+)[^\n]*?
		    (?P<match>
		    @lilypond\s*(
		    \[
		      \s*(?P<options>.*?)\s*
		    \])?\s*{
		      (?P<code>.*?)
		    })''',

		'lilypond_block':
		  r'''(?msx)
		    ^(?P<match>
		    @lilypond\s*(
		    \[
		      \s*(?P<options>.*?)\s*
		    \])?\s+?
		    ^(?P<code>.*?)
		    ^@end\s+lilypond)\s''',

		'lilypond_file':
		  r'''(?mx)
		    ^(?P<match>
		    @lilypondfile\s*(
		    \[
		      \s*(?P<options>.*?)\s*
		    \])?\s*{
		      (?P<filename>\S+)
		    })''',

		'multiline_comment':
		  r'''(?smx)
		    ^(?P<match>
		      (?P<code>
		      @ignore\s
			.*?
		      @end\s+ignore))\s''',

		'singleline_comment':
		  r'''(?mx)
		    ^.*
		    (?P<match>
		      (?P<code>
		      @c([ \t][^\n]*|)\n))''',

	# don't do this: fucks up with @code{@{}
	#	'verb': r'''(?P<code>@code{.*?})''',

		'verbatim':
		  r'''(?sx)
		    (?P<match>
		      (?P<code>
		      @example
			\s.*?
		      @end\s+example\s))''',
	},
}

format_res = {
	HTML: {
		'intertext': r',?\s*intertext=\".*?\"',
		'option_sep': '\s*',
	},

	LATEX: {
		'intertext': r',?\s*intertext=\".*?\"',
		'option_sep': '\s*,\s*',
	},

	TEXINFO: {
		'intertext': r',?\s*intertext=\".*?\"',
		'option_sep': '\s*,\s*',
	},
}

# options without a pattern in ly_options
simple_options = [
	EXAMPLEINDENT,
	FRAGMENT,
	NOFRAGMENT,
	NOINDENT,
	PRINTFILENAME,
	TEXIDOC,
	VERBATIM
]

ly_options = {
	##
	NOTES: {
		RELATIVE: r'''\relative c%(relative_quotes)s''',
	},

	##
	PAPER: {
		INDENT: r'''indent = %(indent)s''',

		LINEWIDTH: r'''linewidth = %(linewidth)s''',

		QUOTE: r'''linewidth = %(linewidth)s - 2.0 * %(exampleindent)s''',

		RAGGEDRIGHT: r'''raggedright = ##t''',
	},

	##
	LAYOUT: {
		NOTIME: r'''\context {
    \Staff
    \remove Time_signature_engraver
  }''',
	},

	##
	PREAMBLE: {
		STAFFSIZE: r'''#(set-global-staff-size %(staffsize)s)''',
	},
}

output = {
	##
	HTML: {
		FILTER: r'''<lilypond %(options)s>
%(code)s
</lilypond>
''',

		AFTER: r'''
  </a>
</p>''',

		BEFORE: r'''<p>
  <a href="%(base)s.ly">''',

		OUTPUT: r'''
    <img align="center" valign="center"
	 border="0" src="%(image)s" alt="[image of music]">''',

		PRINTFILENAME: '<p><tt><a href="%(base)s.ly">%(filename)s</a></tt></p>',

		QUOTE: r'''<blockquote>
%(str)s
</blockquote>
''',

		VERBATIM: r'''<pre>
%(verb)s</pre>''',
	},

	##
	LATEX: {
		OUTPUT: r'''{%%
\parindent 0pt
\catcode`\@=12
\ifx\preLilyPondExample \undefined
  \relax
\else
  \preLilyPondExample
\fi
\def\lilypondbook{}%%
\input %(base)s.tex
\ifx\postLilyPondExample \undefined
  \relax
\else
  \postLilyPondExample
\fi
}''',

		PRINTFILENAME: '''\\texttt{%(filename)s}
	''',

		QUOTE: r'''\begin{quotation}
%(str)s
\end{quotation}
''',

		VERBATIM: r'''\noindent
\begin{verbatim}
%(verb)s\end{verbatim}
''',

		FILTER: r'''\begin{lilypond}[%(options)s]
%(code)s
\end{lilypond}''',
	},

	##
	TEXINFO: {
		FILTER: r'''@lilypond[%(options)s]
%(code)s
@lilypond''',

		OUTPUT: r'''@noindent
@image{%(base)s,,,[image of music],%(ext)s}''',

		PRINTFILENAME: '''@file{%(filename)s}
	''',

		QUOTE: r'''@quotation
%(str)s@end quotation
''',

		NOQUOTE: r'''@format
%(str)s@end format
''',

		VERBATIM: r'''@exampleindent 0
@example
%(verb)s@end example
''',
	},
}

PREAMBLE_LY = r'''%%%% Generated by %(program_name)s
%%%% Options: [%(option_string)s]

#(set! toplevel-score-handler ly:parser-print-score)
#(set! toplevel-music-handler (lambda (p m)
			       (ly:parser-print-score
				p (ly:music-scorify m p))))

%(preamble_string)s

\paper {
  #(define dump-extents #t)
  %(paper_string)s
}

\layout {
  %(layout_string)s
}
'''

FRAGMENT_LY = r'''
%(notes_string)s
{
%% ly snippet contents follows:
%(code)s
%% end ly snippet
}
'''

FULL_LY = '''
%% ly snippet:
%(code)s
%% end ly snippet
'''

texinfo_linewidths = {
	'@afourpaper': '160\\mm',
	'@afourwide': '6.5\\in',
	'@afourlatex': '150\\mm',
	'@smallbook': '5\\in',
	'@letterpaper': '6\\in',
}

def classic_lilypond_book_compatibility (key, value):
	if key == 'singleline' and value == None:
		return (RAGGEDRIGHT, None)

	m = re.search ('relative\s*([-0-9])', key)
	if m:
		return ('relative', m.group (1))

	m = re.match ('([0-9]+)pt', key)
	if m:
		return ('staffsize', m.group (1))

	if key == 'indent' or key == 'linewidth':
		m = re.match ('([-.0-9]+)(cm|in|mm|pt|staffspace)', value)
		if m:
			f = float (m.group (1))
			return (key, '%f\\%s' % (f, m.group (2)))

	return (None, None)

def compose_ly (code, options, type):
	option_dict = {}

	for i in options:
		if string.find (i, '=') > 0:
			(key, value) = re.split ('\s*=\s*', i)
			option_dict[key] = value
		else:
			if i in no_options.keys ():
				if no_options[i] in option_dict.keys ():
					del option_dict[no_options[i]]
			else:
				option_dict[i] = None

	has_linewidth = option_dict.has_key (LINEWIDTH)
	no_linewidth_value = 0

	# if LINEWIDTH is used without parameter, set it to default
	if has_linewidth and option_dict[LINEWIDTH] == None:
		no_linewidth_value = 1
		del option_dict[LINEWIDTH]

	for i in default_ly_options.keys ():
		if i not in option_dict.keys ():
			option_dict[i] = default_ly_options[i]

	if not has_linewidth:
		if type == 'lilypond' or FRAGMENT in option_dict.keys ():
			option_dict[RAGGEDRIGHT] = None

		if type == 'lilypond':
			if LINEWIDTH in option_dict.keys ():
				del option_dict[LINEWIDTH]
		else:
			if RAGGEDRIGHT in option_dict.keys ():
				if LINEWIDTH in option_dict.keys ():
					del option_dict[LINEWIDTH]

		if QUOTE in option_dict.keys () or type == 'lilypond':
			if LINEWIDTH in option_dict.keys ():
				del option_dict[LINEWIDTH]

	if not INDENT in option_dict.keys ():
		option_dict[INDENT] = '0\\mm'

	# the QUOTE pattern from ly_options only emits the `linewidth' keyword
	if has_linewidth and QUOTE in option_dict.keys ():
		if no_linewidth_value:
			del option_dict[LINEWIDTH]
		else:
			del option_dict[QUOTE]

	if FRAGMENT in option_dict.keys ():
		body = FRAGMENT_LY
	else:
		body = FULL_LY

	# defaults
	relative = 1
	override = {}
	# The concept of the `exampleindent' option is broken.  It is not
	# possible to get a sane value for @exampleindent at all without
	# processing the document itself.  Saying
	#
	#   @exampleindent 0
	#   @example
	#   ...
	#   @end example
	#   @exampleindent 5
	#
	# causes ugly results with the DVI backend of texinfo since the
	# default value for @exampleindent isn't 5em but 0.4in (or a smaller
	# value).  Executing the above code changes the environment
	# indentation to an unknown value because we don't know the amount
	# of 1em in advance since it is font-dependent.  Modifying
	# @exampleindent in the middle of a document is simply not
	# supported within texinfo.
	#
	# To set @exampleindent locally to zero, we use the @format
	# environment for non-quoted snippets.
	override[EXAMPLEINDENT] = r'0.4\in'
	override[LINEWIDTH] = texinfo_linewidths['@smallbook']
	override.update (default_ly_options)

	option_list = []
	for (key, value) in option_dict.items ():
		if value == None:
			option_list.append (key)
		else:
			option_list.append (key + '=' + value)
	option_string = string.join (option_list, ',')

	compose_dict = {}
	compose_types = [NOTES, PREAMBLE, LAYOUT, PAPER]
	for a in compose_types:
		compose_dict[a] = []

	for (key, value) in option_dict.items():
		(c_key, c_value) = \
		  classic_lilypond_book_compatibility (key, value)
		if c_key:
			if c_value:
				ly.warning \
				  (_ ("deprecated ly-option used: %s=%s" \
				    % (key, value)))
				ly.warning \
				  (_ ("compatibility mode translation: %s=%s" \
				    % (c_key, c_value)))
			else:
				ly.warning \
				  (_ ("deprecated ly-option used: %s" \
				    % key))
				ly.warning \
				  (_ ("compatibility mode translation: %s" \
				    % c_key))

			(key, value) = (c_key, c_value)

		if value:
			override[key] = value
		else:
			if not override.has_key (key):
				override[key] = None

		found = 0
		for type in compose_types:
			if ly_options[type].has_key (key):
				compose_dict[type].append (ly_options[type][key])
				found = 1
				break

		if not found and key not in simple_options:
			ly.warning (_ ("ignoring unknown ly option: %s") % i)

	# URGS
	if RELATIVE in override.keys () and override[RELATIVE]:
		relative = string.atoi (override[RELATIVE])

	relative_quotes = ''

	# 1 = central C
	if relative < 0:
		relative_quotes += ',' * (- relative)
	elif relative > 0:
		relative_quotes += "'" * relative

	program_name = __main__.program_name

	paper_string = \
	  string.join (compose_dict[PAPER], '\n  ') % override
	layout_string = \
	  string.join (compose_dict[LAYOUT], '\n  ') % override
	notes_string = \
	  string.join (compose_dict[NOTES], '\n  ') % vars ()
	preamble_string = \
	  string.join (compose_dict[PREAMBLE], '\n  ') % override

	return (PREAMBLE_LY + body) % vars ()

# BARF
# use lilypond for latex (.lytex) books,
# and lilypond --preview for html, texinfo books?
def to_eps (file):
	cmd = r'latex "\nonstopmode \input %s"' % file
	# Ugh.  (La)TeX writes progress and error messages on stdout
	# Redirect to stderr
	cmd = '(( %s >&2 ) >&- )' % cmd
	ly.system (cmd)
	ly.system ('dvips -Ppdf -u+ec-mftrace.map -u+lilypond.map -E -o %s.eps %s' \
		   % (file, file))

	# check if it really is EPS.
	# Otherwise music glyphs disappear from 2nd and following pages.

	# TODO: should run dvips -pp -E per page, then we get proper
	# cropping as well.

	f = open ('%s.eps' % file)
	for x in range (0, 10):
		if re.search ("^%%Pages: ", f.readline ()):
			# make non EPS.
			ly.system ('dvips -Ppdf -u+ec-mftrace.map -u+lilypond.map -o %s.eps %s' \
				   % (file, file))
			break

def find_file (name):
	for i in include_path:
		full = os.path.join (i, name)
		if os.path.exists (full):
			return full
	ly.error (_ ("file not found: %s") % name + '\n')
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
	return re.split (format_res[format]['option_sep'], option_string)

class Chunk:
	def replacement_text (self):
		return ''

	def filter_text (self):
		return self.replacement_text ()

	def ly_is_outdated (self):
		return 0

	def png_is_outdated (self):
		return 0

class Substring (Chunk):
	def __init__ (self, source, start, end):
		self.source = source
		self.start = start
		self.end = end

	def replacement_text (self):
		return self.source [self.start:self.end]

class Snippet (Chunk):
	def __init__ (self, type, match, format):
		self.type = type
		self.match = match
		self.hash = 0
		self.options = []
		self.format = format

	def replacement_text (self):
		return self.match.group ('match')

	def substring (self, s):
		return self.match.group (s)

	def __repr__ (self):
		return `self.__class__` + " type = " + self.type

class Include_snippet (Snippet):
	def processed_filename (self):
		f = self.substring ('filename')
		return os.path.splitext (f)[0] + format2ext[format]

	def replacement_text (self):
		s = self.match.group ('match')
		f = self.substring ('filename')

		return re.sub (f, self.processed_filename (), s)

class Lilypond_snippet (Snippet):
	def __init__ (self, type, match, format):
		Snippet.__init__ (self, type, match, format)
		os = match.group ('options')
		if os:
			self.options = split_options (os)

	def ly (self):
		return self.substring ('code')

	def full_ly (self):
		s = self.ly ()
		if s:
			return compose_ly (s, self.options, self.type)
		return ''

	# todo: use md5?
	def get_hash (self):
		if not self.hash:
			self.hash = abs (hash (self.full_ly ()))
		return self.hash

	def basename (self):
		if use_hash_p:
			return 'lily-%d' % self.get_hash ()
		raise 'to be done'

	def write_ly (self):
		outf = open (self.basename () + '.ly', 'w')
		outf.write (self.full_ly ())

		open (self.basename () + '.txt', 'w').write ("image of music")

	def ly_is_outdated (self):
		base = self.basename ()

		tex_file = '%s.tex' % base
		ly_file = '%s.ly' % base
		ok = os.path.exists (ly_file) and os.path.exists (tex_file)\
		     and os.stat (tex_file)[stat.ST_SIZE] \
		     and open (tex_file).readlines ()[-1][1:-1] \
		     == 'lilypondend'

		if ok and (use_hash_p or self.ly () == open (ly_file).read ()):
			# TODO: something smart with target formats
			# (ps, png) and m/ctimes
			return None
		return self

	def png_is_outdated (self):
		base = self.basename ()
		ok = self.ly_is_outdated ()
		if format == HTML or format == TEXINFO:
			ok = ok and (os.path.exists (base + '.png')
				     or glob.glob (base + '-page*.png'))
		return not ok

	def filter_text (self):
		code = self.substring ('code')
		s = run_filter (code)
		d = {
			'code': s,
			'options': self.match.group ('options')
		}
		# TODO
		return output[self.format][FILTER] % d

	def replacement_text (self):
		func = Lilypond_snippet.__dict__ ['output_' + self.format]
		return func (self)

	def get_images (self):
		base = self.basename ()
		# URGUGHUGHUGUGHU
		single = '%(base)s.png' % vars ()
		multiple = '%(base)s-page1.png' % vars ()
		images = (single,)
		if os.path.exists (multiple) \
		   and (not os.path.exists (single) \
			or (os.stat (multiple)[stat.ST_MTIME] \
			    > os.stat (single)[stat.ST_MTIME])):
			images = glob.glob ('%(base)s-page*.png' % vars ())
		return images

	def output_html (self):
		str = ''
		base = self.basename ()
		if format == HTML:
			str += self.output_print_filename (HTML)
			if VERBATIM in self.options:
				verb = verbatim_html (self.substring ('code'))
				str += write (output[HTML][VERBATIM] % vars ())
			if QUOTE in self.options:
				str = output[HTML][QUOTE] % vars ()

		str += output[HTML][BEFORE] % vars ()
		for image in self.get_images ():
			(base, ext) = os.path.splitext (image)
			str += output[HTML][OUTPUT] % vars ()
		str += output[HTML][AFTER] % vars ()
		return str

	def output_info (self):
		str = self.output_print_filename (HTML)
		for image in self.get_images ():
			(base, ext) = os.path.splitext (image)

			# URG, makeinfo implicitly prepends dot to ext
			# specifying no extension is most robust
			ext = ''
			str += output[TEXINFO][OUTPUT] % vars ()
		return str

	def output_latex (self):
		str = ''
		base = self.basename ()
		if format == LATEX:
			str += self.output_print_filename (LATEX)
			if VERBATIM in self.options:
				verb = self.substring ('code')
				str += (output[LATEX][VERBATIM] % vars ())
			if QUOTE in self.options:
				str = output[LATEX][QUOTE] % vars ()

		str += (output[LATEX][OUTPUT] % vars ())
		return str

	def output_print_filename (self,format):
		str = ''
		if PRINTFILENAME in self.options:
			base = self.basename ()
			filename = self.substring ('filename')
			str = output[format][PRINTFILENAME] % vars ()
		return str

	def output_texinfo (self):
		str = ''
		if self.output_print_filename (TEXINFO):
			str += ('@html\n' + self.output_print_filename (HTML)
				+ '\n@end html\n')
			str += ('@tex\n' + self.output_print_filename (LATEX)
				+ '\n@end tex\n')
		base = self.basename ()
		if TEXIDOC in self.options:
			texidoc = base + '.texidoc'
			if os.path.exists (texidoc):
				str += '@include %(texidoc)s\n\n' % vars ()

		if VERBATIM in self.options:
			verb = verbatim_texinfo (self.substring ('code'))
			str += (output[TEXINFO][VERBATIM] % vars ())
			if not QUOTE in self.options:
				str = output[TEXINFO][NOQUOTE] % vars()

		str += ('@ifinfo\n' + self.output_info () + '\n@end ifinfo\n')
		str += ('@tex\n' + self.output_latex () + '\n@end tex\n')
		str += ('@html\n' + self.output_html () + '\n@end html\n')

		if QUOTE in self.options:
			str = output[TEXINFO][QUOTE] % vars ()

		# need par after image
		str += '\n'

		return str

class Lilypond_file_snippet (Lilypond_snippet):
	def ly (self):
		name = self.substring ('filename')
		return '\\renameinput \"%s\"\n%s' % (name, open (find_file (name)).read ())

snippet_type_to_class = {
	'lilypond_file': Lilypond_file_snippet,
	'lilypond_block': Lilypond_snippet,
	'lilypond': Lilypond_snippet,
	'include': Include_snippet,
}

def find_toplevel_snippets (s, types):
	res = {}
	for i in types:
		res[i] = ly.re.compile (snippet_res[format][i])

	snippets = []
	index = 0
	## found = dict (map (lambda x: (x, None), types))
	## urg python2.1
	found = {}
	map (lambda x, f = found: f.setdefault (x, None), types)

	# We want to search for multiple regexes, without searching
	# the string multiple times for one regex.
	# Hence, we use earlier results to limit the string portion
	# where we search.
	# Since every part of the string is traversed at most once for
	# every type of snippet, this is linear.

	while 1:
		first = None
		endex = 1 << 30
		for type in types:
			if not found[type] or found[type][0] < index:
				found[type] = None
				m = res[type].search (s[index:endex])
				if not m:
					continue

				cl = Snippet
				if snippet_type_to_class.has_key (type):
					cl = snippet_type_to_class[type]
				snip = cl (type, m, format)
				start = index + m.start ('match')
				found[type] = (start, snip)

			if found[type] \
			   and (not first or found[type][0] < found[first][0]):
				first = type

				# FIXME.

				# Limiting the search space is a cute
				# idea, but this *requires* to search
				# for possible containing blocks
				# first, at least as long as we do not
				# search for the start of blocks, but
				# always/directly for the entire
				# @block ... @end block.

				endex = found[first][0]

		if not first:
			snippets.append (Substring (s, index, len (s)))
			break

		(start, snip) = found[first]
		snippets.append (Substring (s, index, start))
		snippets.append (snip)
		found[first] = None
		index = start + len (snip.match.group ('match'))

	return snippets

def filter_pipe (input, cmd):
	if verbose_p:
		ly.progress (_ ("Opening filter `%s'") % cmd)

	(stdin, stdout, stderr) = os.popen3 (cmd)
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
		ly.error (_ ("`%s' failed (%d)") % (cmd, exit_status))
		ly.error (_ ("The error log is as follows:"))
		sys.stderr.write (error)
		sys.stderr.write (stderr.read ())
		ly.exit (status)

	if verbose_p:
		ly.progress ('\n')

	return output

def run_filter (s):
	return filter_pipe (s, filter_cmd)

def is_derived_class (cl, baseclass):
	if cl == baseclass:
		return 1
	for b in cl.__bases__:
		if is_derived_class (b, baseclass):
			return 1
	return 0

def process_snippets (cmd, ly_snippets, png_snippets):
	ly_names = filter (lambda x: x, map (Lilypond_snippet.basename, ly_snippets))
	png_names = filter (lambda x: x, map (Lilypond_snippet.basename, png_snippets))

	status = 0
	if ly_names:
		status = ly.system (string.join ([cmd] + ly_names),
				    ignore_error = 1, progress_p = 1)

	if status:
		ly.error ('Process %s exited unsuccessfully.' % cmd)
		raise Compile_error

	if format == HTML or format == TEXINFO:
		for i in png_names:
			if not os.path.exists (i + '.eps') and os.path.exists (i + '.tex'):
				to_eps (i)
				ly.make_ps_images (i + '.eps', resolution = 110)

#			elif os.path.exists (i + '.ps'):
#				ly.make_ps_images (i + '.ps', resolution = 110)

LATEX_DOCUMENT = r'''
%(preamble)s
\begin{document}
\typeout{textwidth=\the\textwidth}
\typeout{columnsep=\the\columnsep}
\makeatletter\if@twocolumn\typeout{columns=2}\fi\makeatother
\end{document}
'''
# need anything else besides textwidth?
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
	m = re.search ('textwidth=([0-9.]*)pt', parameter_string)
	if m:
		textwidth = string.atof (m.group (1))
		if columns:
			textwidth = (textwidth - columnsep) / columns

	return textwidth

ext2format = {
	'.html': HTML,
	'.itely': TEXINFO,
	'.latex': LATEX,
	'.lytex': LATEX,
	'.tely': TEXINFO,
	'.tex': LATEX,
	'.texi': TEXINFO,
	'.texinfo': TEXINFO,
	'.xml': HTML,
}

format2ext = {
	HTML: '.html',
	# TEXINFO: '.texinfo',
	TEXINFO: '.texi',
	LATEX: '.tex',
}

class Compile_error:
	pass

def do_process_cmd (chunks):
	ly_outdated = filter (lambda x: is_derived_class (x.__class__, Lilypond_snippet) \
			and x.ly_is_outdated (), chunks)
	png_outdated = filter (lambda x: is_derived_class (x.__class__, Lilypond_snippet) \
			 and x.png_is_outdated (), chunks)

	ly.progress (_ ("Writing snippets..."))
	map (Lilypond_snippet.write_ly, ly_outdated)
	ly.progress ('\n')

	if ly_outdated:
		ly.progress (_ ("Processing...\n"))
		process_snippets (process_cmd, ly_outdated, png_outdated)
	else:
		ly.progress (_ ("All snippets are up to date..."))
	ly.progress ('\n')

def do_file (input_filename):
	# ugh
	global format
	if not format:
		e = os.path.splitext (input_filename)[1]
		if e in ext2format.keys ():
			# FIXME
			format = ext2format[e]
		else:
			ly.error (_ ("cannot determine format for: %s" \
				     % input_filename))
			ly.exit (1)

	if not input_filename or input_filename == '-':
		in_handle = sys.stdin
		input_fullname = '<stdin>'
	else:
		if os.path.exists (input_filename):
			input_fullname = input_filename
		elif format == LATEX:
			# urg python interface to libkpathsea?
			input_fullname = ly.read_pipe ('kpsewhich '
						       + input_filename)[:-1]
		else:
			input_fullname = find_file (input_filename)
		in_handle = open (input_fullname)

	if input_filename == '-':
		input_base = 'stdin'
	else:
		input_base = os.path.basename \
			     (os.path.splitext (input_filename)[0])

	# only default to stdout when filtering
	if output_name == '-' or (not output_name and filter_cmd):
		output_filename = '-'
		output_file = sys.stdout
	else:
		if not output_name:
			output_filename = input_base + format2ext[format]
		else:
			if not os.path.isdir (output_name):
				os.mkdir (output_name, 0777)
			output_filename = (output_name
					   + '/' + input_base
					   + format2ext[format])

		if (os.path.exists (input_filename) and
		    os.path.exists (output_filename) and
		    os.path.samefile (output_filename, input_fullname)):
			ly.error (_ ("Output would overwrite input file; use --output."))
			ly.exit (2)

		output_file = open (output_filename, 'w')
		if output_name:
			os.chdir (output_name)
	try:
		ly.progress (_ ("Reading %s...") % input_fullname)
		source = in_handle.read ()
		ly.progress ('\n')

		# FIXME: containing blocks must be first, see
		#        find_toplevel_snippets
		snippet_types = (
			'multiline_comment',
			'verbatim',
			'lilypond_block',
	#		'verb',
			'singleline_comment',
			'lilypond_file',
			'include',
			'lilypond', )
		ly.progress (_ ("Dissecting..."))
		chunks = find_toplevel_snippets (source, snippet_types)
		ly.progress ('\n')

		global default_ly_options
		textwidth = 0
		if not default_ly_options.has_key (LINEWIDTH):
			if format == LATEX:
				textwidth = get_latex_textwidth (source)
				default_ly_options[LINEWIDTH] = \
				  '''%.0f\\pt''' % textwidth
			elif format == TEXINFO:
				for (k, v) in texinfo_linewidths.items ():
					# FIXME: @layout is usually not in chunk #0:
					#        \input texinfo @c -*-texinfo-*-
					# bluntly search first K of source
					# s = chunks[0].replacement_text ()
					if re.search (k, source[:1024]):
						default_ly_options[LINEWIDTH] = v
						break

		if filter_cmd:
			output_file.writelines ([c.filter_text () for c in chunks])

		elif process_cmd:
			do_process_cmd (chunks)
			ly.progress (_ ("Compiling %s...") % output_filename)
			output_file.writelines ([s.replacement_text () \
						 for s in chunks])
			ly.progress ('\n')

		def process_include (snippet):
			os.chdir (original_dir)
			name = snippet.substring ('filename')
			ly.progress (_ ("Processing include: %s") % name)
			ly.progress ('\n')
			do_file (name)

		map (process_include,
		     filter (lambda x: is_derived_class (x.__class__, Include_snippet), chunks))
	except Compile_error:
		os.chdir (original_dir)
		ly.progress (_ ("Removing `%s'") % output_filename)
		ly.progress ('\n')

		os.unlink (output_filename)
		raise Compile_error

def do_options ():
	global format, output_name
	global filter_cmd, process_cmd, verbose_p

	(sh, long) = ly.getopt_args (option_definitions)
	try:
		(options, files) = getopt.getopt (sys.argv[1:], sh, long)
	except getopt.error, s:
		sys.stderr.write ('\n')
		ly.error (_ ("getopt says: `%s'" % s))
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
	if process_cmd == '':
		process_cmd = lilypond_binary + " -f tex"

	if process_cmd:
		process_cmd += string.join ([(' -I %s' % p)
					     for p in include_path])

	ly.identify (sys.stderr)
	ly.setup_environment ()
	if files:
		try:
			do_file (files[0])
		except Compile_error:
			ly.exit (1)

if __name__ == '__main__':
	main ()
