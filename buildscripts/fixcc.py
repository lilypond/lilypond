#!/usr/bin/python

# fixcc -- nitpick lily's c++ code

# TODO
#  * check lexer, parser
#  * rewrite in elisp, add to cc-mode
#  * ?
#  * profit

import __main__
import getopt
import os
import re
import string
import sys
import time

COMMENT = 'COMMENT'
CXX = 'C++'

rules = {
	CXX:
	[
	# space before parenthesis open
	('([^\( \]])[ \t]*\(', '\\1 ('),
	# space after comma
	(',[ \t]*', ', '),
	# delete inline tabs
	('(\w)\t+', '\\1 '),
	# delete inline double spaces
	('   *', ' '),
	# delete space after parenthesis open
	('\([ \t]*', '('),
	# delete space before parenthesis close
	('[ \t]*\)', ')'),
	# delete spaces after prefix
	('(--|\+\+)[ \t]*([\w\)])', '\\1\\2'),
	# delete spaces before postfix
	('([\w\)\]])[ \t]*(--|\+\+)', '\\1\\2'),
	# delete space after parenthesis close
	#('\)[ \t]*([^\w])', ')\\1'),
	# delete superflous space around operator
	('([\w\)\]])([ \t]+)(&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|<|>|\+|-|=|/|&|\|\*)([ \t]+)([\w\(])', '\\1 \\3 \\5'),
	# space around operator
	('([\w\)\]])(&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|<|>|=|/|&|\|\*)([\w\(])', '\\1 \\2 \\3'),
	# space around +/-; exponent
	('([\w\)\]])(\+|-)([_A-Za-z\(])', '\\1 \\2 \\3'),
	('([_\dA-Za-df-z\)\]])(\+|-)([\w\(])', '\\1 \\2 \\3'),
	# trailing operator
	(' (&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|<|>|\+|-|=|/|\*XXX)[ \t]*\n([ \t]*)',
	 '\n\\2\\1 '),
	# pointer
	('(bool|char|const|int|unsigned|void|([A-Z]\w*))[ \t]*(\*|&)[ \t]*',
	 '\\1 \\3'),
	# unary pointer, minus, not
	('(return|=) (\*|&|-|!) ([\w\(])', '\\1 \\2\\3'),
	# space after `operator'
	('(\Woperator) (\W)', '\\1\\2'),
	# dangling newline
	('\n[ \t]*\n[ \t]*\n', '\n\n'),
	# dangling parenthesis open
	#('[ \t]*\n[ \t]*\([ \t]*\n', '('),
	('\([ \t]*\n', '('),
	# dangling parenthesis close
	('\n[ \t]*\)', ')'),
	# dangling comma
	('\n[ \t]*,', ','),
	# dangling semicolon
	('\n[ \t]*;', ';'),
	# brace open
	('(\w[^\n]*){[ \t]*\n', '\\1\n{\n'),
	# brace open backslash
	('(\w[^\n]*){[ \t]*\\\\\n', '\\1\\\n{\\\n'),
	# brace close
	('}[ \t]*([^\n]*\w[^\n\\\]*\n)', '}\n\\1\n'),
	# brace close backslash
	('}[ \t]*([^\n]*\w[^\n]*?\\\\\n)', '}\\\n\\1\n'),
	# delete space before comma
	('[ \t]*,', ','),
	# delete space before semicolon
	('[ \t]*;', ';'),
	# delete space before eol-backslash
	('[ \t]*\\\\\n', '\\\n'),
	# delete trailing whitespace
	('[ \t]*\n', '\n'),

	## Massage code that gets broken by rules above.
	# delete spaces around template brackets
	('(dynamic_cast|template|([A-Z]\w*))[ \t]*<[ \t]*((bool|char|int|unsigned|void|(class[ \t]+\w*)|([A-Z]\w*)))[ \t]*?(| \*)[ \t]*>',
	 '\\1<\\3\\7>'),
	# delete space before #define x()
	('#[ \t]*define (\w*)[ \t]*\(', '#define \\1('),
	# add space in #define x ()
	('#[ \t]*define (\w*)(\(([^\(\)]|\([^\(\)]*\))*\)\\n)',
	 '#define \\1 \\2'),
	# delete space in #include <>
	('#[ \t]*include[ \t]*<[ \t]*([^ \t>]*)[ \t]*(/?)[ \t]*([^ \t>]*)[ \t]*>',	 
	'#include <\\1\\2\\3>'),
	# delete backslash before empty line (emacs' indent region is broken)
	('\\\\\n\n', '\n\n'),
	],

	COMMENT:
	[
	# delete trailing whitespace
	('[ \t]*\n', '\n'),
	# delete empty first lines
	('(/\*\n)\n*', '\\1'),
	# delete empty last lines
	('\n*(\n\*/)', '\\1'),
	## delete newline after start?
	#('/(\*)\n', '\\1'),
	## delete newline before end?
	#('\n(\*/)', '\\1'),
	],
	}


# Recognize special sequences in the input.
#
#   (?P<name>regex) -- Assign result of REGEX to NAME.
#   *? -- Match non-greedily.
#   (?m) -- Multiline regex: Make ^ and $ match at each line.
#   (?s) -- Make the dot match all characters including newline.
#   (?x) -- Ignore whitespace in patterns.
no_match = 'a\ba'
snippet_res = {
	CXX: {
		'include':
		  no_match,

		'multiline_comment':
		  r'''(?sx)
		    (?P<match>
		    (?P<code>
		    [ \t]*/\*.*?\*/))''',

		'singleline_comment':
		  r'''(?mx)
		    ^.*
		    (?P<match>
		      (?P<code>
		      [ \t]*//([ \t][^\n]*|)\n))''',

		'string':
		  r'''(?x)
		    (?P<match>
		    (?P<code>
		    "([^"]|(([^\\]|(\\\\))\\"))*"))''',

		'char':
		  r'''(?x)
		    (?P<match>
		    (?P<code>
		    '([^']+|\')))''',

		'include':
		  r'''(?x)
		    (?P<match>
		    (?P<code>
		    "#[ \t]*include[ \t]*<[^>]*>''',
	},
	}

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
		s = self.source[self.start:self.end]
		for i in rules[CXX]:
			s = re.sub (i[0], i[1], s)
		return s
		

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
		return `self.__class__` + ' type = ' + self.type

class Multiline_comment (Snippet):
	def __init__ (self, source, match, format):
		self.type = type
		self.match = match
		self.hash = 0
		self.options = []
		self.format = format

	def replacement_text (self):
		s = self.match.group ('match')
		for i in rules[COMMENT]:
			s = re.sub (i[0], i[1], s)
		return s

snippet_type_to_class = {
	'multiline_comment': Multiline_comment,
#	'lilypond_block': Lilypond_snippet,
#	'lilypond': Lilypond_snippet,
#	'include': Include_snippet,
}

def find_toplevel_snippets (s, types):
	res = {}
	for i in types:
		res[i] = re.compile (snippet_res[format][i])

	snippets = []
	index = 0
	## found = dict (map (lambda x: (x, None),
	##		      types))
	## urg python2.1
	found = {}
	map (lambda x, f = found: f.setdefault (x, None),
	     types)

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
			   and (not first \
				or found[type][0] < found[first][0]):
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

def nitpick_file (outdir, file):
	s = open (file).read ()

	# FIXME: Containing blocks must be first, see
	#        find_toplevel_snippets.
	snippet_types = (
		'multiline_comment',
		'singleline_comment',
		'string',
		'char',
		)

	chunks = find_toplevel_snippets (s, snippet_types)
	#code = filter (lambda x: is_derived_class (x.__class__, Substring),
	#	       chunks)

	t = string.join (map (lambda x: x.filter_text (), chunks), '')
	fixt = file
	if s != t:
		if not outdir:
			os.system ('mv %s %s~' % (file, file))
		else: 
			fixt = os.path.join (outdir,
					     os.path.basename (file))
		h = open (fixt, "w")
		h.write (t)
		h.close ()
	indent_file (fixt)

def indent_file (file):
	emacs = '''emacs\
	--no-window-system\
	--batch\
	--no-site-file\
	--no-init-file\
	%(file)s\
	--eval '(let ((error nil)
		      (version-control nil))
		 (load-library "cc-mode")
		 (c++-mode)
		 (indent-region (point-min) (point-max))
		 (if (buffer-modified-p (current-buffer))
		  (save-buffer)))' ''' % vars ()
	emacsclient = '''emacsclient\
	--socket-name=%(socketdir)s/%(socketname)s\
        --no-wait\
	--eval '(let ((error nil)
		      (version-control nil))
		 (load-library "cc-mode")
                 (find-file "%(file)s")
		 (c++-mode)
		 (indent-region (point-min) (point-max))
		 (if (buffer-modified-p (current-buffer))
		  (save-buffer)))' ''' \
		  % { 'file': file,
		      'socketdir' : socketdir,
		      'socketname' : socketname, }
	os.system (emacs)


def usage ():
	sys.stdout.write (r'''
Usage:
fixcc [--outdir=DIR] FILE...

Typical use with LilyPond:

   fixcc $(find flower kpath-guile lily -name '*cc' -o -name '*hh' | grep -v /out)

This script is licensed under the GNU GPL
''')

def do_options ():
	global outdir
	(options, files) = getopt.getopt (sys.argv[1:], '',
					  ['help', 'outdir='])
	for (o, a) in options:
		if o == '--help':
			usage ()
			sys.exit (0)
		elif o == '--outdir':
			outdir = a
		else:
			assert unimplemented
	if not files:
		usage ()
		sys.exit (2)
	return files


outdir = 0
format = CXX
socketdir = '/tmp/fixcc'
socketname = 'fixcc%d' % os.getpid ()

def setup_client ():
	#--no-window-system\
	#--batch\
	os.unlink (os.path.join (socketdir, socketname))
	os.mkdir (socketdir, 0700)
	emacs='''emacs\
		--no-site-file\
		--no-init-file\
		--eval '(let ((error nil)
			      (version-control nil))
			 (load-library "server")
			 (setq server-socket-dir "%(socketdir)s")
			 (setq server-name "%(socketname)s")
			 (server-start)
			 (while t) (sleep 1000))' ''' \
			 % { 'socketdir' : socketdir,
			     'socketname' : socketname, }
			     
	if not os.fork ():
		os.system (emacs)
		sys.exit (0)
	while not os.path.exists (os.path.join (socketdir, socketname)):
		time.sleep (1)

def main ():
	#emacsclient should be faster, but this does not work yet
	#setup_client ()
	files = do_options ()
	if outdir and not os.path.isdir (outdir):
		os.makedirs (outdir)
	for i in files:
                sys.stderr.write ('%s...\n' % i)
		nitpick_file (outdir, i)

if __name__ == '__main__':
	main ()

