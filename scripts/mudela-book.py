#!@PYTHON@
# vim: set noexpandtab:
import time
t1 = time.clock()

# support bruk av convert-mudela
#
# option:
# 11pt, 13pt, 16pt, 20pt, 26pt
# singleline
# multiline
# fragment  (used when a comment containg \score confuses mudela-book)
# nonfragment (probably not needed)
# verbatim

# latex only options:
# eps
# 

# command line options
# --defalt-mudela-fontsize
# --force-mudela-fontsize
# --outname
# --force-verbatim make all mudela verbatim. Maybe not that useful
# --dependencies
# --dep-prefix
# --no-pictures
# --no-lily
# TODO: Figure out clean set of options.

# BUG: does not handle \verb|\begin{verbatim}\end{verbatim}| correctly.
# Should make a joint RE for \verb and \begin, \end{verbatim}

# TODO: add an option to read the .ly files from a previous run and dump
# the .tex file, so you can do
#
# * mudela-book file.tex
# * convert-mudela *.ly
# * mudela-book --read-lys *.ly
#

import os
import stat
import string
import re
import getopt
import sys
import __main__


initfile = ''
program_version = '1.3.69-very-unstable'

include_path = [os.getcwd()]

g_dep_prefix = ''
g_outdir = ''
g_force_mudela_fontsize = 0
g_read_lys = 0
g_do_pictures = 1
g_num_cols = 1
format = ''
g_run_lilypond = 1
g_use_hash = 1
no_match = 'a\ba'

default_music_fontsize = 16
default_text_fontsize = 12

# latex linewidths:
# indices are no. of columns, papersize,  fontsize
# Why can't this be calculated?
latex_linewidths = {
 1: {'a4':{10: 345, 11: 360, 12: 390},
	 'a5':{10: 276, 11: 276, 12: 276},
	 'b5':{10: 345, 11: 356, 12: 356},
	 'letter':{10: 345, 11: 360, 12: 390},
	 'legal': {10: 345, 11: 360, 12: 390},
	 'executive':{10: 345, 11: 360, 12: 379}},
 2: {'a4':{10: 167, 11: 175, 12: 190},
	 'a5':{10: 133, 11: 133, 12: 133},
	 'b5':{10: 167, 11: 173, 12: 173},
	 'letter':{10: 167, 11: 175, 12: 190},
	 'legal':{10: 167, 11: 175, 12: 190},
	 'executive':{10: 167, 11: 175, 12: 184}}}

texi_linewidths = {
	'a4': {12: 455},
	'a4wide': {12: 470},
	'smallbook': {12: 361},
	'texidefault': {12: 433}}


def get_linewidth(cols, paper, fontsize):
	if __main__.format == 'latex':
		return latex_linewidths[cols][paper][fontsize]
	elif __main__.format == 'texi':
		return texi_linewidths[paper][fontsize]
	raise "never here"

option_definitions = [
  ('EXT', 'f', 'format', 'set format.  EXT is one of texi and latex.'),
  ('DIM',  '', 'default-music-fontsize', 'default fontsize for music.  DIM is assumed to in points'),
  ('DIM',  '', 'default-mudela-fontsize', 'deprecated, use --default-music-fontsize'),
  ('', 'h', 'help', 'print help'),
  ('DIR', 'I', 'include', 'include path'),
  ('', '', 'init', 'mudela-book initfile'),
  ('DIM', '', 'force-music-fontsize', 'force fontsize for all inline mudela. DIM is assumed to in points'),
  ('DIM', '', 'force-mudela-fontsize', 'deprecated, use --force-music-fontsize'),
  ('', '', 'force-verbatim', 'make all mudela verbatim'),
  ('', 'M', 'dependencies', 'write dependencies'),
  ('', 'n', 'no-lily', 'don\'t run lilypond'),
  ('', '', 'no-pictures', "don\'t generate pictures"),
  ('', '', 'read-lys', "don't write ly files."),
  ('FILE', 'o', 'outname', 'prefix for filenames'),
  ('', 'v', 'version', 'print version information' ),
  ('PREF', '',  'dep-prefix', 'prepend PREF before each -M dependency'),
  ('FILE', '', 'outdir', "where to place generated files"),
  ]

# format specific strings, ie. regex-es for input, and % strings for output
output_dict= {
	'latex': {
		'output-mudela-fragment' : r"""\begin[eps,singleline,%s]{mudela}
  \context Staff <
    \context Voice{
      %s
    }
  >
\end{mudela}""", 
		'output-mudela':r"""\begin[%s]{mudela}
%s
\end{mudela}""",
		'output-verbatim': r"""\begin{verbatim}%s\end{verbatim}""",
		'output-default-post': r"""\def\postMudelaExample{}""",
		'output-default-pre': r"""\def\preMudelaExample{}""",
		'output-eps': '\\noindent\\parbox{\\mudelaepswidth{%(fn)s.eps}}{\includegraphics{%(fn)s.eps}}',
		'output-tex': '\\preMudelaExample \\input %(fn)s.tex \\postMudelaExample\n',
		'pagebreak': r'\pagebreak',
		},
	'texi' : {'output-mudela': """@mudela[%s]
%s
@end mudela 
""",
		  'output-mudela-fragment': """@mudela[%s]
\context Staff\context Voice{ %s }
@end mudela """,
		  'pagebreak': None,
		  'output-verbatim': r"""@example
%s
@end example
""",

# do some tweaking: @ is needed in some ps stuff.
# override EndLilyPondOutput, since @tex is done
# in a sandbox, you can't do \input lilyponddefs at the
# top of the document.

# should also support fragment in
		  
		  'output-all': r"""@tex
\catcode`\@=12
\input lilyponddefs
\def\EndLilyPondOutput{}
\input %(fn)s.tex
\catcode`\@=0
@end tex
@html
<p>
<img src=%(fn)s.png>
@end html
""",
		}
	}

def output_verbatim (body):#ugh .format
	if __main__.format == 'texi':
		body = re.sub ('([@{}])', '@\\1', body)
	return get_output ('output-verbatim') % body

def output_mbverbatim (body):#ugh .format
	if __main__.format == 'texi':
		body = re.sub ('([@{}])', '@\\1', body)
	return get_output ('output-verbatim') % body

re_dict = {
	'latex': {'input': '\\\\mbinput{?([^}\t \n}]*)',
		  'include': '\\\\mbinclude{(?P<filename>[^}]+)}',
		 
		  'option-sep' : ', *',
		  'header': r"""\\documentclass(\[.*?\])?""",
		  'preamble-end': '\\\\begin{document}',
		  'verbatim': r"""(?s)\\begin{verbatim}(?P<code>.*?)\\end{verbatim}""",
		  'verb': r"""\\verb(.)(?P<code>.*?)\1""",
		  'mudela-file': r'\\mudelafile(\[(?P<options>.*?)\])?\{(?P<filename>.+)}',
		  'mudela' : '\\\\mudela(\[(?P<options>.*?)\])?{(?P<code>.*?)}',
		  'mudela-block': r"""(?s)\\begin(\[(?P<options>.*?)\])?{mudela}(?P<code>.*?)\\end{mudela}""",
		  'interesting-cs': '\\\\(chapter|section|twocolumn|onecolumn)',
		  'def-post-re': r"""\\def\\postMudelaExample""",
		  'def-pre-re': r"""\\def\\preMudelaExample""",		  
		  'intertext': r',?\s*intertext=\".*?\"',
		  'ignore': no_match,
		  'numcols': r"(?P<code>\\(?P<num>one|two)column)",
		  },
	
	'texi': {
		 'include':  '@mbinclude[ \n\t]+(?P<filename>[^\t \n]*)',
		 'input': no_match,
		 'header': no_match,
		 'preamble-end': no_match,
		 'verbatim': r"""(?s)(?P<code>@example\s.*?@end example\s)""",
		 'verb': r"""@code{(?P<code>.*?)}""",
		 'mudela-file': '@mudelafile(\[(?P<options>.*?)\])?{(?P<filename>[^}]+)}',
		 'mudela' : '@mudela(\[(?P<options>.*?)\])?{(?P<code>.*?)}',
		 'mudela-block': r"""(?s)@mudela(\[(?P<options>.*?)\])?\s(?P<code>.*?)@end mudela\s""",
		 'interesting-cs': r"""[\\@](chapter|section)""",
		  'option-sep' : ', *',
		  'intertext': r',?\s*intertext=\".*?\"',
		  'ignore': r"(?s)@ignore\s(.*?)@end ignore\s",
		  'numcols': no_match,
		 }
	}


for r in re_dict.keys ():
	olddict = re_dict[r]
	newdict = {}
	for k in olddict.keys ():
		newdict[k] = re.compile (olddict[k])
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
	try:
		fd = open(fname)
	except IOError:
		error ("Error opening `%s'" % fname)
	str = fd.read ()
	s = re.search('%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)', str)
	if s:
		return (int(s.group(3))-int(s.group(1)), 
			int(s.group(4))-int(s.group(2)))
	else:
		return (0,0)


def error (str):
	sys.stderr.write (str + "\n  Exiting ... \n\n")
	raise 'Exiting.'


def compose_full_body (body, opts):
	"""Construct the mudela code to send to Lilypond.
	Add stuff to BODY using OPTS as options."""
	if __main__.format == 'texi':
		paper = 'texidefault'
	else:
		paper = 'letter' # yes, latex use letter as default, at least
		                 # my tetex distro
	music_size = default_music_fontsize
	latex_size = default_text_fontsize
	for o in opts:
		m = re.search ('^(.*)paper$', o)
		if m:
			paper = m.group (1)
		
		if g_force_mudela_fontsize:
			music_size = g_force_mudela_fontsize
		else:
			m = re.match ('([0-9]+)pt', o)
			if m:
				music_size = string.atoi(m.group (1))

		m = re.match ('latexfontsize=([0-9]+)pt', o)
		if m:
			latex_size = string.atoi (m.group (1))

	if re.search ('\\\\score', body):
		is_fragment = 0
	else:
		is_fragment = 1
	if 'fragment' in opts:
		is_fragment = 1
	if 'nonfragment' in opts:
		is_fragment = 0

	if is_fragment and not 'multiline' in opts:
		opts.append('singleline')
	if 'singleline' in opts:
		l = -1.0;
	else:
		l = get_linewidth(g_num_cols, paper, latex_size)
	
	if 'relative' in opts:#ugh only when is_fragment
		body = '\\relative c { %s }' % body
	
	if is_fragment:
		body = r"""\score { 
 \notes { %s }
  \paper { }  
}""" % body

	opts = uniq (opts)
	optstring = string.join (opts, ' ')
	optstring = re.sub ('\n', ' ', optstring)
	
	body = r"""
%% Generated by mudela-book.py; options are %s  %%ughUGH not original options
\include "paper%d.ly"
\paper  { linewidth = %f \pt; } 
""" % (optstring, music_size, l) + body
	return body


def scan_preamble (str):
	options = []
	if __main__.format == 'texi':
		x = 250
		if string.find(str[:x], "@afourpaper") != -1:
			options = ['a4paper']
		elif string.find(str[:x], "@afourwide") != -1:
			options = ['a4widepaper']
		elif string.find(str[:x], "@smallbook") != -1:
			options = ['smallbookpaper']
	m = get_re ('header').search( str)
	# should extract paper & fontsz.
	if m and m.group (1):
		options = options + re.split (',[\n \t]*', m.group(1)[1:-1])

	def verbose_fontsize ( x):
		if re.match('[0-9]+pt', x):
			return 'latexfontsize=' + x
		else:
			return x 
			
	options = map (verbose_fontsize, options)
	return options


def completize_preamble (str):
	m = get_re ('preamble-end').search( str)
	if not m:
		return str
	
	preamble = str [:m.start (0)]
	str = str [m.start(0):]
	
	if not get_re('def-post-re').search (preamble):
		preamble = preamble + get_output('output-default-post')
	if not get_re ('def-pre-re').search(  preamble):
		preamble = preamble + get_output ('output-default-pre')

	# UGH ! BUG!
	#if  re.search ('\\\\includegraphics', str) and not re.search ('usepackage{graphics}',str):

	preamble = preamble + '\\usepackage{graphics}\n'

	return preamble + str


read_files = []
def find_file (name):
	f = None
	for a in include_path:
		try:
			nm = os.path.join (a, name)
			f = open (nm)
			__main__.read_files.append (nm)
			break
		except IOError:
			pass
	if f:
		return f.read ()
	else:
		error ("File not found `%s'\n" % name)
		return ''

def do_ignore(match_object):
	return []

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

def make_mudela(m):
	if m.group('options'):
		options = m.group('options')
	else:
		options = ''
	return [('input', get_output('output-mudela-fragment') % 
			(options, m.group('code')))]

def make_mudela_file(m):
	if m.group('options'):
		options = m.group('options')
	else:
		options = ''
	return [('input', get_output('output-mudela') %
			(options, find_file(m.group('filename'))))]

def make_mudela_block(m):
	if m.group('options'):
		options = get_re('option-sep').split (m.group('options'))
	else:
	    options = []
	options = filter(lambda s: s != '', options)
	if 'mbverbatim' in options:#ugh this is ugly and only for texi format
		s  = m.group()
		im = get_re('intertext').search(s)
		if im:
			s = s[:im.start()] + s[im.end():]
		im = re.search('mbverbatim', s)
		if im:
			s = s[:im.start()] + s[im.end():]
		if s[:9] == "@mudela[]":
			s = "@mudela" + s[9:]
		return [('mudela', m.group('code'), options, s)]
	return [('mudela', m.group('code'), options)]

def do_columns(m):
	if __main__.format != 'latex':
		return []
	if m.group('num') == 'one':
		return [('numcols', m.group('code'), 1)]
	if m.group('num') == 'two':
		return [('numcols', m.group('code'), 2)]
	
def chop_chunks(chunks, re_name, func):
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
                    newchunks.append (('input', str[:m.start (0)]))
                    newchunks.extend(func(m))
                    str = str [m.end(0):]
        else:
            newchunks.append(c)
    return newchunks

def read_doc_file (filename):
	"""Read the input file, find verbatim chunks and do \input and \include
	"""
	str = ''
	str = find_file(filename)

	if __main__.format == '':
		latex =  re.search ('\\\\document', str[:200])
		texinfo =  re.search ('@node|@setfilename', str[:200])
		if (texinfo and latex) or not (texinfo or latex):
			error("error: can't determine format, please specify")
		if texinfo:
			__main__.format = 'texi'
		else:
			__main__.format = 'latex'
	chunks = [('input', str)]
	# we have to check for verbatim before doing include,
	# because we don't want to include files that are mentioned
	# inside a verbatim environment
	chunks = chop_chunks(chunks, 'ignore', do_ignore)
	chunks = chop_chunks(chunks, 'verbatim', make_verbatim)
	chunks = chop_chunks(chunks, 'verb', make_verb)
	#ugh fix input
	chunks = chop_chunks(chunks, 'include', do_include_file)
	chunks = chop_chunks(chunks, 'input', do_input_file)
	return chunks


def advance_counters (counter, str):
	"""Advance chap/sect counters,
	Return the new counter tuple
	"""
	(chapter, section, count) = counter
	while str:
		m = get_re ('interesting-cs').search(str)
		if not m:
			break
		str = str[m.end(0):]
		g = m.group (1)
		if g == 'chapter':#ugh use dict
			(chapter, section, count)  = (chapter + 1, 0, 0)
		elif g == 'section':
			(section, count)  = (section + 1, 0)
	return (chapter, section, count)

taken_file_names = []
def schedule_mudela_block (basename, chunk, extra_opts):
	"""Take the body and options from CHUNK, figure out how the
	real .ly should look, and what should be left MAIN_STR (meant
	for the main file).  The .ly is written, and scheduled in
	TODO.

	Return: a chunk (TYPE_STR, MAIN_STR, OPTIONS, TODO, BASE)

	TODO has format [basename, extension, extension, ... ]
	
	"""
	if len(chunk) == 3:
		(type, body, opts) = chunk
		complete_body = None
	else:# mbverbatim
		(type, body, opts, complete_body) = chunk
	assert type == 'mudela'
	opts = opts +  extra_opts
	file_body = compose_full_body (body, opts)
	if __main__.g_use_hash:
		basename = `abs(hash (file_body))`
	for o in opts:
		m = re.search ('filename="(.*?)"', o)
		if m:
			basename = m.group (1)#ugh add check if more than
			#one file has the same name
			assert basename not in taken_file_names
			taken_file_names.append(basename)
	# writes the file if necessary, returns true if it was written
	if not g_read_lys:
		update_file(file_body, os.path.join(g_outdir, basename) + '.ly')
	needed_filetypes = ['tex']

	if format  == 'texi':
		needed_filetypes.append('eps')
		needed_filetypes.append('png')
	if 'eps' in opts and not ('eps' in needed_filetypes):
		needed_filetypes.append('eps')
	outname = os.path.join(g_outdir, basename)
	if not os.path.isfile(outname + '.tex') \
		or os.stat(outname+'.ly')[stat.ST_MTIME] > \
			os.stat(outname+'.tex')[stat.ST_MTIME]:
		todo = needed_filetypes
	else:
		todo = []
		
	newbody = ''
	if 'verbatim' in opts:
		newbody = output_verbatim (body)
	elif 'mbverbatim' in opts:
		newbody = output_mbverbatim (complete_body)

	for o in opts:
		m = re.search ('intertext="(.*?)"', o)
		if m:
			newbody = newbody  + m.group (1)
	if format == 'latex':
		if 'eps' in opts:
			s = 'output-eps'
		else:
			s = 'output-tex'
	else: # format == 'texi'
		s = 'output-all'
	newbody = newbody + get_output(s) % {'fn': basename }
	return ('mudela', newbody, opts, todo, basename)

def process_mudela_blocks(outname, chunks, global_options):#ugh rename
	(chap,sect,count) = (0,0,0)
	newchunks = []
	# Count sections/chapters.
	for c in chunks:
		if c[0] == 'input':
			(chap,sect,count) = advance_counters((chap,sect,count), c[1])
		elif c[0] == 'mudela':
			base = '%s-%d.%d.%d' % (outname, chap, sect, count)
			count = count + 1
			c = schedule_mudela_block (base, c, global_options)
		elif c[0] == 'numcols':
			__main__.g_num_cols = c[2]
		newchunks.append (c)
	return newchunks


def find_eps_dims (match):
	"Fill in dimensions of EPS files."
	
	fn =match.group (1)
	dims = bounding_box_dimensions (fn)

	return '%ipt' % dims[0]


def system (cmd):
	sys.stderr.write ("invoking `%s'\n" % cmd)
	st = os.system (cmd)
	if st:
		error ('Error command exited with value %d\n' % st)
	return st

def compile_all_files (chunks):
	eps = []
	tex = []
	png = []

	for c in chunks:
		if c[0] <> 'mudela':
			continue
		base  = c[4]
		exts = c[3]
		for e in exts:
			if e == 'eps':
				eps.append (base)
			elif e == 'tex':
				tex.append (base + '.ly')
			elif e == 'png' and g_do_pictures:
				png.append (base)
	d = os.getcwd()
	if g_outdir:
		os.chdir(g_outdir)
	if tex:
		lilyopts = map (lambda x:  '-I ' + x, include_path)
		lilyopts = string.join (lilyopts, ' ' )
		texfiles = string.join (tex, ' ')
		system ('lilypond %s %s' % (lilyopts, texfiles))
	for e in eps:
		system(r"tex '\nonstopmode \input %s'" % e)
		system(r"dvips -E -o %s %s" % (e + '.eps', e))
	for g in png:
		cmd = r"""gs -sDEVICE=pgm  -dTextAlphaBits=4 -dGraphicsAlphaBits=4  -q -sOutputFile=- -r90 -dNOPAUSE %s -c quit | pnmcrop | pnmtopng > %s"""
		cmd = cmd % (g + '.eps', g + '.png')
		system (cmd)
	if g_outdir:
		os.chdir(d)


def update_file (body, name):
	"""
	write the body if it has changed
	"""
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
	sys.stdout.write("""Usage: mudela-book [options] FILE\n
Generate hybrid LaTeX input from Latex + mudela
Options:
""")
	sys.stdout.write (options_help_str (option_definitions))
	sys.stdout.write (r"""Warning all output is written in the CURRENT directory



Report bugs to bug-gnu-music@gnu.org.

Written by Tom Cato Amundsen <tca@gnu.org> and
Han-Wen Nienhuys <hanwen@cs.uu.nl>
""")

	sys.exit (0)


def write_deps (fn, target):
	sys.stdout.write('writing `%s\'\n' % os.path.join(g_outdir, fn))
	f = open (os.path.join(g_outdir, fn), 'w')
	f.write ('%s%s: ' % (g_dep_prefix, target))
	for d in __main__.read_files:
		f.write ('%s ' %  d)
	f.write ('\n')
	f.close ()
	__main__.read_files = []

def identify():
	sys.stdout.write ('mudela-book (GNU LilyPond) %s\n' % program_version)

def print_version ():
	identify()
	sys.stdout.write (r"""Copyright 1998--1999
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.
""")

def do_file(input_filename):
	file_settings = {}
	if outname:
		my_outname = outname
	else:
		my_outname = os.path.basename(os.path.splitext(input_filename)[0])
	my_depname = my_outname + '.dep'		

	chunks = read_doc_file(input_filename)
	chunks = chop_chunks(chunks, 'mudela', make_mudela)
	chunks = chop_chunks(chunks, 'mudela-file', make_mudela_file)
	chunks = chop_chunks(chunks, 'mudela-block', make_mudela_block)
	chunks = chop_chunks(chunks, 'numcols', do_columns)
	#for c in chunks: print c, "\n"
	global_options = scan_preamble(chunks[0][1])
	chunks = process_mudela_blocks(my_outname, chunks, global_options)
	# Do It.
	if __main__.g_run_lilypond:
		compile_all_files (chunks)
		newchunks = []
		# finishing touch.
		for c in chunks:
			if c[0] == 'mudela' and 'eps' in c[2]:
				body = re.sub (r"""\\mudelaepswidth{(.*?)}""", find_eps_dims, c[1])
				newchunks.append (('mudela', body))
			else:
				newchunks.append (c)
		chunks = newchunks

	if chunks and chunks[0][0] == 'input':
		chunks[0] = ('input', completize_preamble (chunks[0][1]))

	foutn = os.path.join(g_outdir, my_outname + '.' + format)
	sys.stderr.write ("Writing `%s'\n" % foutn)
	fout = open (foutn, 'w')
	for c in chunks:
		#if c[1] is not None:
			fout.write (c[1])
	fout.close ()

	if do_deps:
		write_deps (my_depname, foutn)


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
	elif o == '--version':
		print_version ()
		sys.exit  (0)

	elif o == '--format' or o == '-o':
		__main__.format = a
	elif o == '--outname' or o == '-o':
		if len(files) > 1:
			#HACK
			sys.stderr.write("Mudela-book is confused by --outname on multiple files")
			sys.exit(1)
		outname = a
	elif o == '--help' or o == '-h':
		help ()
	elif o == '--no-lily' or o == '-n':
		__main__.g_run_lilypond = 0
	elif o == '--dependencies':
		do_deps = 1
	elif o == '--default-music-fontsize':
		default_music_fontsize = string.atoi (a)
	elif o == '--default-mudela-fontsize':
		print "--default-mudela-fontsize is deprecated, use --default-music-fontsize"
		default_music_fontsize = string.atoi (a)
	elif o == '--force-music-fontsize':
		g_force_mudela_fontsize = string.atoi(a)
	elif o == '--force-mudela-fontsize':
		print "--force-mudela-fontsize is deprecated, use --default-mudela-fontsize"
		g_force_mudela_fontsize = string.atoi(a)

	elif o == '--init':
		initfile =  a
	elif o == '--dep-prefix':
		g_dep_prefix = a
	elif o == '--no-pictures':
		g_do_pictures = 0
	elif o == '--read-lys':
		g_read_lys = 1
	elif o == '--outdir':
		g_outdir = a

identify()
if g_outdir:
	if os.path.isfile(g_outdir):
		error ("outdir is a file: %s" % g_outdir)
	if not os.path.exists(g_outdir):
		os.mkdir(g_outdir)
for input_filename in files:
	do_file(input_filename)
	


t2 = time.clock()
print "Time:", t2-t1
#
# Petr, ik zou willen dat ik iets zinvoller deed,
# maar wat ik kan ik doen, het verandert toch niets?
#   --hwn 20/aug/99
