#!@PYTHON@

import os
import string
import re
import getopt
import sys
import __main__

outdir = 'out'
initfile = ''
program_version = '@TOPLEVEL_VERSION@'

cwd = os.getcwd ()
include_path = [cwd]

# TODO: Figure out clean set of options.

# BUG: does not handle \verb|\begin{verbatim}\end{verbatim}| correctly.
# Should make a joint RE for \verb and \begin, \end{verbatim}
#

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


options = [
  ('DIM',  '', 'default-mudela-fontsize', 'default fontsize for music.  DIM is assumed to in points'),
  ('EXT', 'f', 'format', 'set format.  EXT is one of texi and latex.'),
  ('', 'h', 'help', 'print help'),
  ('DIR', 'I', 'include', 'include path'),
  ('', '', 'init', 'mudela-book initfile'),
#  ('DIM', '', 'force-mudela-fontsize', 'force fontsize for all inline mudela. DIM is assumed to in points'),
  ('', '', 'force-verbatim', 'make all mudela verbatim'),
  ('', 'M', 'dependencies', 'write dependencies'),
  ('', 'n', 'no-lily', 'don\'t run lilypond'),
  ('FILE', 'o', 'outname', 'prefix for filenames'),
  ('', 'v', 'version', 'print version information' )
  ]

format = ''
run_lilypond = 1
no_match = 'a\ba'

# format specific strings, ie. regex-es for input, and % strings for output
output_dict= {
	'latex': {
		'output-mudela-fragment' : r"""\begin[eps,fragment%s]{mudela}
  \context Staff <
    \context Voice{
      %s
    }
  >
\end{mudela}""", 
		'output-mudela':r"""\begin%s{mudela}
%s
\end{mudela}""",
		'output-verbatim': r"""\begin{verbatim}%s\end{verbatim}""",
		'output-default-post': r"""\def\postMudelaExample{}""",
		'output-default-pre': r"""\def\preMudelaExample{}""",
		'output-eps': '\\noindent\\parbox{\\mudelaepswidth{%s.eps}}{\includegraphics{%s.eps}}',
		'output-tex': '\\preMudelaExample \\input %s.tex \\postMudelaExample\n'
		},
	'texi' : {'output-mudela': """@mudela[%s]
%s
@end mudela
""",
		  'output-verbatim': r"""@example
%s
@end example
""",

# do some tweaking: @ is needed in some ps stuff.
# override EndLilyPondOutput, since @tex is done
# in a sandbox, you can't do \input lilyponddefs at the
# top of the document.
		  'output-all': r"""@tex
\catcode`\@=12
\input lilyponddefs
\def\EndLilyPondOutput{}
\input %s.tex
\catcode`\@=0
@end tex
@html
<img src=%s.png>
@end html
""",
		}
	}

def output_verbatim (body):
	if __main__.format == 'texi':
		body = re.sub ('([@{}])', '@\\1', body)
	return get_output ('output-verbatim') % body

re_dict = {
	'latex': {'input': '\\\\input{?([^}\t \n}]*)',
		  'include': '\\\\include{([^}]+)}',
		 
		  'comma-sep' : ', *',
		  'header': r"""\\documentclass(\[.*?\])?""",
		  'preamble-end': '\\\\begin{document}',
		  'verbatim': r"""(?s)\\begin{verbatim}(.*?)\\end{verbatim}""",
		  'verb': r"""\\verb(.)(.*?)\1""",
		  'mudela-file': '\\\\mudelafile(\[[^\\]]+\])?{([^}]+)}',
		  'mudela' : '\\\\mudela(\[.*?\])?{(.*?)}',
		  'mudela-block': r"""(?s)\\begin(\[.*?\])?{mudela}(.*?)\\end{mudela}""",
		  'interesting-cs': '\\\\(chapter|section|twocolumn|onecolumn)',
		  'def-post-re': r"""\\def\\postMudelaExample""",
		  'def-pre-re': r"""\\def\\preMudelaExample""",		  
		  },
	'texi': {'input': '@include[ \n\t]+([^\t \n]*)',
		 'include': no_match,
		 'header': no_match,
		 'preamble-end': no_match,
		 'verbatim': r"""(?s)@example(.*?)@end example$""",
		 'verb': r"""@code{(.*?)}""",
		 'mudela-file': '@mudelafile(\[[^\\]]+\])?{([^}]+)}',
		 'mudela' : '@mudela(\[.*?\])?{(.*?)}',
		 'mudela-block': r"""(?s)@mudela(\[.*?\])?(.*?)@end mudela""",
		 'interesting-cs': r"""[\\@](node|mudelagraphic)""",
		  'comma-sep' : ', *',		 
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
	str = fd.read (-1)
	s = re.search('%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)', str)
	if s:
		return (int(s.group(3))-int(s.group(1)), 
			int(s.group(4))-int(s.group(2)))
	else:
		return (0,0)


def find_file (name):
	for a in include_path:
		try:
			nm = os.path.join (a, name)
			f = open (nm)
			return nm
		except IOError:
			pass
	return ''

def error (str):
	sys.stderr.write (str + "\n  Exiting ... \n\n")
	raise 'Exiting.'


def compose_full_body (body, opts):
	"Construct the text of an input file: add stuff to BODY using OPTS as options."
	paper = 'a4'
	music_size = default_music_fontsize
	latex_size = default_text_fontsize

	cols = 1
	for o in opts:
		m = re.search ('^(.*)paper$', o)
		if m:
			paper = m.group (1)
		

		m = re.match ('([0-9]+)pt', o)
		if m:
			music_size = string.atoi(m.group (1))

		m = re.match ('latexfontsize=([0-9]+)pt', o)
		if m:
			latex_size = string.atoi (m.group (1))


	if 'twocolumn' in opts:
		cols = 2
		

	# urg: breaks on \include of full score
	# Use nofly option if you want to \include full score.
	if not 'nofly' in opts and not re.search ('\\\\score', body):
		opts.append ('fly')

	if  'fragment' in opts or 'singleline' in opts:
		l = -1.0;
	else:
		l = latex_linewidths[cols][paper][latex_size]


	if 'fly' in opts:
		body = r"""\score { 
  \notes\relative c {
    %s
  }
  \paper { }  
}""" % body

        opts = uniq (opts)
        optstring = string.join (opts, ' ')
	optstring = re.sub ('\n', ' ', optstring)
		
	body = r"""
%% Generated by mudela-book.py; options are %s
\include "paper%d.ly"
\paper  { linewidth = %f \pt; } 
""" % (optstring, music_size, l) + body

	return body

def find_inclusion_chunks (regex, surround, str):
	chunks = []
	while str:
		m = regex.search (str)

		if m == None:
			chunks.append (('input', str))
			str = ''
			break

		chunks.append (('input', str[: m.start (0)]))
		chunks.append (('input', surround))
		chunks = chunks + read_doc_file (m.group (1))
		chunks.append (('input', surround))

		str = str [m.end (0):]
	return chunks

def find_include_chunks (str):
	return find_inclusion_chunks (get_re ('include'), '\\newpage', str)

def find_input_chunks (str):
	return find_inclusion_chunks (get_re ('input'), '', str)	

def read_doc_file (filename):
	"""Read the input file, substituting for \input, \include, \mudela{} and \mudelafile"""
	str = ''
	for ext in ['', '.tex', '.doc', '.tely']:
		try:
			f = open(filename+ ext)
			str = f.read (-1)
		except:
			pass
		

	if not str:
		error ("File not found `%s'\n" % filename)

	retdeps =  [filename]

	if __main__.format == '':
		latex =  re.search ('\\\\document', str[:200])
		texinfo =  re.search ('@node', str[:200])
		if (texinfo and latex) or not (texinfo or latex):
			error("error: can't determine format, please specify")
		if texinfo:
			__main__.format = 'texi'
		else:
			__main__.format = 'latex'
			
	chunks = [('input', str)]

	for func in (find_verbatim_chunks, find_verb_chunks, find_include_chunks, find_input_chunks):
		newchunks = []
		for c in chunks:
			if c[0] == 'input':
				ch = func (c[1])
				newchunks = newchunks + ch
			else:
				newchunks.append (c)
		chunks = newchunks
	
	return chunks



def scan_preamble (str):
	options = []
	m = get_re ('header').search( str)

	# should extract paper & fontsz.
	if m and m.group (1):
		options = options + re.split (',[\n \t]*', m.group(1)[1:-1])

	def verbose_fontsize ( x):
		# o ??
		#if o.match('[0-9]+pt'):
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

def find_verbatim_chunks (str):
	"""Chop STR into a list of tagged chunks, ie. tuples of form
	(TYPE_STR, CONTENT_STR), where TYPE_STR is one of 'input' and 'verbatim'
	"""

	chunks = []
	while str:
		m = get_re ('verbatim').search( str)
		if m == None:
			chunks.append( ('input', str))
			str = ''
		else:
			chunks.append (('input', str[:m.start (0)]))
			chunks.append (('verbatim', m.group (0)))
		
			str = str [m.end(0):]
		
	return chunks	      

def find_verb_chunks (str):

	chunks = []
	while str:
		m = get_re ("verb").search(str)
		if  m == None:
			chunks.append (('input', str))
			str = ''
		else:
			chunks.append (('input', str[:m.start (0)]))
			chunks.append (('verbatim', m.group (0)))
			str = str [m.end(0):]

	return chunks
			


def find_mudela_shorthand_chunks (str):
	return [('input', find_mudela_shorthands(str))]
	
def find_mudela_shorthands (b):
	def mudela_short (match):
		"Find \mudela{}, and substitute appropriate \begin / \end blocks."
		opts = match.group (1)
		if opts:
			opts = ',' + opts[1:-1]
		else:
			opts = ''
		return get_output ('output-mudela-fragment')  % (opts, match.group (2))

	def mudela_file (match):
		"Find \mudelafile, and substitute appropriate \begin / \end blocks."
		d = [] #, d = retdeps
		full_path = find_file (match.group (2))
		if not full_path:
			error("error: can't find file `%s'\n" % match.group(2))

		d.append (full_path)
		f = open (full_path)
		str = f.read (-1)
		opts = match.group (1)
		if opts:
			opts = re.split (',[ \n\t]*', opts[1:-1])
		else:
			opts = []

		if re.search ('.fly$', full_path):
			opts.append ('fly')
		elif re.search ('.sly$', full_path):
			opts = opts + [ 'fly','fragment']
		elif re.search ('.ly$', full_path):
			opts .append ('nofly')
			
		str_opts = string.join (opts, ',')
		if str_opts: str_opts = '[' + str_opts + ']'


		str = ("%% copied from file `%s'\n" % full_path) + str 
		return get_output ('output-mudela') % (str_opts, str)
  
	b = get_re('mudela-file').sub (mudela_file, b)
	b = get_re('mudela').sub (mudela_short, b)
	return b
	
def find_mudela_chunks (str):
	"""Find mudela blocks, while watching for verbatim. Returns
	(STR,MUDS) with \mudelagraphic substituted for the blocks in STR,
	and the blocks themselves MUDS"""
  
	chunks = []
	while str:
		m = get_re ("mudela-block").search( str)
		if not m:
			chunks.append (('input', str))
			str = ''
			break

		chunks.append (('input', str[:m.start (0)]))
		
		opts = m.group (1)
  		if opts:
  			opts = opts[1:-1]
  		else:
  			opts = ''
		optlist = get_re('comma-sep').split (opts)
		
		body = m.group (2)
		chunks.append (('mudela', body, optlist))
  
		str = str [m.end (0):]
  
	return chunks
  
  
  
def advance_counters (counter, opts, str):
	"""Advance chap/sect counters,
	revise OPTS. Return the new counter tuple"""
	
	(chapter, section, count) = counter
  	done = ''
  	while str:
		m = get_re ('interesting-cs').search(str)
		if not m:
			done = done + str
			str = ''
			break
		
		done = done + str[:m.end (0)]
		str = str[m.end(0):]
		g = m.group (1)

		if g == 'twocolumn':
			opts.append ('twocolumn')
		elif g  == 'onecolumn':
			try:
				current_opts.remove ('twocolumn')
			except IndexError:
				pass
		elif g == 'chapter':
			(chapter, section, count)  = (chapter + 1, 0, 0)
		elif g == 'section' or g == 'node':
			(section, count)  = (section + 1, 0)
			

	return (chapter, section, count)


def schedule_mudela_block (base, chunk, extra_opts):
	"""Take the body and options from CHUNK, figure out how the
	real .ly should look, and what should be left MAIN_STR (meant
	for the main file).  The .ly is written, and scheduled in
	TODO.

	Return: a chunk (TYPE_STR, MAIN_STR, OPTIONS, TODO)

	TODO has format [basename, extension, extension, ... ]
	
	"""

	(type, body, opts) = chunk
	assert type == 'mudela'
	opts = opts +  extra_opts
	
	newbody = ''
	if 'verbatim' in opts:
		newbody = output_verbatim (body)

	file_body = compose_full_body (body, opts)
	updated = update_file (file_body, base + '.ly')
	todo = [base]			# UGH.

	if not os.path.isfile (base + '.tex') or updated:
		todo.append ('tex')
		updated = 1

	for o in opts:
		m = re.search ('intertext="(.*?)"', o)
		if m:
			newbody = newbody  + m.group (1)

	if format == 'texi':
		opts.append ('png')

	if 'png' in opts:
		opts.append ('eps')

	if 'eps' in opts and ('tex' in todo or
			      not os.path.isfile (base + '.eps')):
		todo.append ('eps')

	if 'png' in opts and ('eps' in todo or
			      not os.path.isfile (base + '.png')):
		todo.append ('png')

	if format == 'latex':
		if 'eps' in opts :
			newbody = newbody + get_output ('output-eps') %  (base, base)
		else:
			newbody = newbody + get_output ('output-tex') % base

	elif format == 'texi':
		newbody = newbody + get_output ('output-all') % (base, base) 


	
	return ('mudela', newbody, opts, todo)

def find_eps_dims (match):
	"Fill in dimensions of EPS files."
	
	fn =match.group (1)
	dims = bounding_box_dimensions (fn)

	return '%ipt' % dims[0]


def print_chunks (ch):
	for c in ch:
		print '-->%s\n%s' % (c[0], c[1])
		if len (c) > 2:
			print '==>%s' % list (c[2:])
	print foo


def transform_input_file (in_filename, out_filename):
	"""Read the input, and deliver a list of chunks
	ready for writing.
	"""

	chunks = read_doc_file (in_filename)

	#.  Process \mudela and \mudelafile.
	for func in [find_mudela_shorthand_chunks,
		     find_mudela_chunks]:
		newchunks = []
		for c in chunks:
			if c[0] == 'input':
				newchunks = newchunks + func (c[1])
			else:
				newchunks.append (c)
		chunks = newchunks

	opts = []
	if chunks:
		opts = scan_preamble (chunks[0][1]) 
	
	(chap,sect,count) = (0,0,0)
	newchunks = []
	# Count sections/chapters.
	for c in chunks:
		if c[0] == 'input':
			(chap,sect,count) = advance_counters((chap,sect,count), opts, c[1])
		elif c[0] == 'mudela':
			base = '%s-%d.%d.%d' % (out_filename, chap, sect, count)
			count = count + 1
			c = schedule_mudela_block (base, c, opts)
			
		newchunks.append (c)

	chunks = newchunks
	newchunks = []

	# Do It.
	if __main__.run_lilypond:
		compile_all_files (chunks)
		
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

	return chunks

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
		base  = c[3][0]
		exts = c[3][1:]
		for e in exts:
			if e == 'eps':
				eps.append (base)
			elif e == 'tex':
				tex.append (base + '.ly')
			elif e == 'png':
				png.append (base)

	if tex:
		lilyopts = map (lambda x:  '-I ' + x, include_path)
		lilyopts = string.join (lilyopts, ' ' )
		texfiles = string.join (tex, ' ')
		system ('lilypond %s %s' % (lilyopts, texfiles))

	for e in eps:
		cmd = r"""tex %s; dvips -E -o %s %s""" % \
		      (e, e + '.eps', e)
		system (cmd)

	for g in png:
		cmd = r"""gs -sDEVICE=pgm  -dTextAlphaBits=4 -dGraphicsAlphaBits=4  -q -sOutputFile=- -r90 -dNOPAUSE %s -c quit | pnmcrop | pnmtopng > %s"""

		cmd = cmd % (g + '.eps', g + '.png')
		system (cmd)

	
def update_file (body, name):
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
	sys.stdout.write (options_help_str (options))
	sys.stdout.write (r"""Warning all output is written in the CURRENT directory



Report bugs to bug-gnu-music@gnu.org.

Written by Tom Cato Amundsen <tomcato@xoommail.com> and
Han-Wen Nienhuys <hanwen@cs.uu.nl>
""")

	sys.exit (0)


def write_deps (fn, target,  deps):
	sys.stdout.write('writing `%s\'\n' % fn)

	f = open (fn, 'w')
	
	target = target + '.latex'
	f.write ('%s: %s\n'% (target, string.join (deps, ' ')))
	f.close ()

		
def identify():
	sys.stdout.write ('mudela-book (GNU LilyPond) %s\n' % program_version)

def print_version ():
	identify()
	sys.stdout.write (r"""Copyright 1998--1999
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.
""")


def main():
	global outdir, initfile, defined_mudela_cmd, defined_mudela_cmd_re
	outname = ''
	try:
		(sh, long) = getopt_args (__main__.options)
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
		elif o == '--outdir' or o == '-d':
			outdir = a
		elif o == '--help' or o == '-h':
			help ()
		elif o == '--no-lily' or o == '-n':
			__main__.run_lilypond = 0
		elif o == '--dependencies':
			do_deps = 1
		elif o == '--default-mudela-fontsize':
			default_music_fontsize = string.atoi (a)
		elif o == '--init':
			initfile =  a
	
	identify()

	for input_filename in files:
		file_settings = {}
		if outname:
			my_outname = outname
		else:
			my_outname = os.path.basename(os.path.splitext(input_filename)[0])
		my_depname = my_outname + '.dep'		

		chunks = transform_input_file (input_filename, my_outname)
		
		foutn = my_outname + '.' + format
		sys.stderr.write ("Writing `%s'\n" % foutn)
		fout = open (foutn, 'w')
		for c in chunks:
			fout.write (c[1])
		fout.close ()

		if do_deps:
			# write_deps (my_depname, my_outname, deps)
			sys.stderr.write ("--dependencies broken")

main()



#
# Petr, ik zou willen dat ik iets zinvoller deed,
# maar wat ik kan ik doen, het verandert toch niets?
#   --hwn 20/aug/99
