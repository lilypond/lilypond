# -*-python-*-

import os
import string

Import ('env')

def verbose_opt (env, opt):
	if env['verbose']:
		return opt
	return ''

srcdir = env['srcdir']
build = env['build']
def join_path (path, infix=os.pathsep, prefix = ''):
	def dir (x):
		if x and x[0] == '#':
			return srcdir + x[1:]
		return x
	return string.join (map (lambda x: prefix + dir (x), path), infix)

verbose = verbose_opt (env, ' --verbose')
MAKEINFO_INCLUDES = join_path (env['MAKEINFO_PATH'], '', ' -I')
MAKEINFO = env['MAKEINFO']
a = ('%(MAKEINFO)s%(verbose)s %(MAKEINFO_INCLUDES)s'\
     ' --no-split --no-headers --output=$TARGET $SOURCE') % vars ()
texi2txt = Builder (action = a, suffix = '.txt', src_suffix = '.texi')
env.Append (BUILDERS = {'Texi2txt': texi2txt})


PYTHON = env['PYTHON']
ABC2LY_PY = env['ABC2LY_PY']
LILYPOND_PY = env['LILYPOND_PY']
LILYPOND_BIN = env['LILYPOND_BIN']
LILYPOND_BOOK = env['LILYPOND_BOOK']
LILYPOND_BOOK_FLAGS = env['LILYPOND_BOOK_FLAGS']
LILYPOND_BOOK_FORMAT = env['LILYPOND_BOOK_FORMAT']
LILYPOND_BOOK_INCLUDES = join_path (env['LILYPOND_BOOK_PATH'], '',
				    ' --include=')
LILYPONDPREFIX = env['LILYPONDPREFIX']

env.Append (ENV = {'PATH' : os.environ['PATH']})
if os.environ.has_key ('LD_LIBRARY_PATH'):
	env.Append (ENV = {'LD_LIBRARY_PATH' : os.environ['LD_LIBRARY_PATH']})
if os.environ.has_key ('GUILE_LOAD_PATH'):
	env.Append (ENV = {'GUILE_LOAD_PATH' : os.environ['GUILE_LOAD_PATH']})

if os.environ.has_key ('TEXMF'):
	env.Append (ENV = {'TEXMF' : os.environ['TEXMF']})
env.Append (ENV = {'TEXMF' : '{' + LILYPONDPREFIX + ',' \
		   + os.popen ('kpsexpand \$TEXMF').read ()[:-1] + '}' })

verbose = verbose_opt (env, ' --verbose')
a = (r'''rm -f $$(grep -LF '\lilypondend' $$(dirname $TARGET)/lily-*.tex 2>/dev/null); ''' \
     + 'LILYPONDPREFIX=%(LILYPONDPREFIX)s '\
     + '%(PYTHON)s %(LILYPOND_BOOK)s%(verbose)s'\
     + ' --include=$$(dirname $TARGET) %(LILYPOND_BOOK_INCLUDES)s'\
     + r""" --process='%(LILYPOND_BIN)s %(LILYPOND_BOOK_INCLUDES)s'"""\
     + ' --output=$$(dirname $TARGET) --format=%(LILYPOND_BOOK_FORMAT)s\
     %(LILYPOND_BOOK_FLAGS)s\
     $SOURCE') % vars ()
tely2texi = Builder (action = a, suffix = '.texi', src_suffix = '.tely')
env.Append (BUILDERS = {'Tely2texi': tely2texi})

TEXINFO_PAPERSIZE_OPTION = env['TEXINFO_PAPERSIZE_OPTION']
a = '(cd $$(dirname $TARGET) &&\
 texi2dvi --batch %(TEXINFO_PAPERSIZE_OPTION)s $$(basename $SOURCE))' % vars ()
texi2dvi = Builder (action = a, suffix = '.dvi', src_suffix = '.texi')
env.Append (BUILDERS = {'Texi2dvi': texi2dvi})

env.Append (DVIPSFLAGS = '-Ppdf -u+lilypond.map -u+ec-mftrace.map')

DVIPS_PAPERSIZE = 'a4'
DVIPSFLAGS = env['DVIPSFLAGS']
a = ('set -x; dvips %(DVIPSFLAGS)s' \
     + ' -o ${TARGET}.pdfps'\
     + ' -t %(DVIPS_PAPERSIZE)s $SOURCE &&'\
     + ' ps2pdf -sPAPERSIZE=%(DVIPS_PAPERSIZE)s ${TARGET}.pdfps $TARGET') \
     % vars ()
dvi2pdf = Builder (action = a, suffix = '.pdf', src_suffix = '.dvi')
env.Append (BUILDERS = {'Dvi2pdf': dvi2pdf})

a = 'convert $SOURCE $TARGET'
png2eps = Builder (action = a, suffix = '.eps', src_suffix = '.png')
env.Append (BUILDERS = {'Png2eps': png2eps})

def add_ps_target (target, source, env):
	base = os.path.splitext (str (target[0]))[0]
	return (target + [base + '.ps'], source)

a = ('LILYPONDPREFIX=%(LILYPONDPREFIX)s '\
     + '%(PYTHON)s %(LILYPOND_PY)s%(verbose)s'\
     + ' --include=$$(dirname $TARGET)'\
     + ' --output=$$(dirname $TARGET)'\
     + ' $SOURCE') % vars ()
lilypond = Builder (action = a, suffix = '.pdf', src_suffix = '.ly')
##		    emitter = add_ps_target)
env.Append (BUILDERS = {'LilyPond': lilypond})

verbose = verbose_opt (env, ' --verbose')
a = ('LILYPONDPREFIX=%(LILYPONDPREFIX)s '\
     + '%(PYTHON)s %(ABC2LY_PY)s%(verbose)s'\
     + ' --include=$$(dirname $TARGET)'\
     + ' --output=$$(dirname $TARGET)'\
     + ' $SOURCE') % vars ()
abc2ly = Builder (action = a, suffix = '.ly', src_suffix = '.abc')
env.Append (BUILDERS = {'Abc2ly': abc2ly})


MFMODE = env['MFMODE']
def add_log_target (target, source, env):
	base = os.path.splitext (str (target[0]))[0]
	return (target + [base + '.log'], source)

def add_enc_ly_tex_target (target, source, env):
	base = os.path.splitext (str (target[0]))[0]
	return (target + [base + '.enc', base + '.tex', base + 'list.ly'],
		source)

def add_suffixes (target, source, env, target_suffixes, src_suffixes):
	base = os.path.splitext (str (target[0]))[0]
	return (target + map (lambda x: base + x, target_suffixes),
		source + map (lambda x: base + x, src_suffixes))

#outdir = os.path.join (env['build'], reldir, env['out'])
outdir = '$$(dirname $TARGET)'
scrdir = env['srcdir']
#MFINPUTS = '.:' + str (Dir ('#/mf'))
#MFINPUTS = '.:$$(dirname $SOURCE)'
a = ('(cd $$(dirname $TARGET) &&'\
#     + ' MFINPUTS=.:$$(dirname $SOURCE)'\
     + ' MFINPUTS=.:$$(dirname $SOURCE):%(srcdir)s/$$(dirname $SOURCE)'\
     + ' mf "\\mode:=%(MFMODE)s; nonstopmode;'\
     + ' input $$(basename $SOURCE);" ' \
     + ' | grep -v "@\|>>")') % vars ()
tfm = Builder (action = a, suffix = '.tfm', src_suffix = '.mf',
#	       emitter = lambda t, s, e: add_suffixes (t, s, e, ['.log'], []))
	       emitter = add_log_target)
env.Append (BUILDERS = {'TFM': tfm})

MF_TO_TABLE_PY = env['MF_TO_TABLE_PY']
#verbose = verbose_opt (env, ' --verbose')
verbose = ''
a = ('%(PYTHON)s %(MF_TO_TABLE_PY)s%(verbose)s'\
     + ' --outdir=$$(dirname $TARGET)'\
     + ' --afm=%(outdir)s/$$(basename $TARGET .afm).afm' \
     + ' --enc=%(outdir)s/$$(basename $TARGET .afm).enc' \
     + ' --tex=%(outdir)s/$$(basename $TARGET .afm).tex' \
     + ' --ly=%(outdir)s/$$(basename $TARGET .afm)list.ly'\
     + ' $$(dirname $TARGET)/$$(basename $SOURCE)') % vars ()
afm = Builder (action = a, suffix = '.afm', src_suffix = '.log',
	       emitter = add_enc_ly_tex_target)
env.Append (BUILDERS = {'AFM': afm})

def add_enc_src (target, source, env):
	base = os.path.splitext (str (target[0]))[0]
	return (target, source + [base + '.enc'])

def encoding_opt (target):
	base = os.path.splitext (os.path.basename (str (target[0])))[0]
	enc = base + '.enc'
	if os.path.exists (os.path.join (outdir, enc)):
		return ' --encoding=' + enc
	return ''

# UGH, should fix --output option for mftrace
xpfa = Builder (action = ('MFINPUTS=.:' + str (Dir ('#/mf')) \
			 + ' mftrace -I %(outdir)s --pfa' \
			 + ' --simplify --keep-trying' \
			 + ' $$(basename $SOURCE .mf) ' \
			 + ' && mv $$(basename $TARGET) $TARGET') % vars (),
	       suffix = '.pfa',
	       src_suffix = '.mf',
	       emitter = add_enc_src)

verbose = verbose_opt (env, ' --verbose')
def run_mftrace (target, source, env):
	TARGET = target[0]
	SOURCE = source[0]
	mf = os.path.basename (str (source[0]))
	base = os.path.splitext (os.path.basename (str (target[0])))[0]
	enc = base + '.enc'
	mfdir = os.path.join (here, reldir)
	outdir = os.path.join (env['build'], reldir, env['out'])
	encoding = encoding_opt (target)
	command = ('(cd $$(dirname $TARGET) && '
		   + ' MFINPUTS=.:$$(dirname $TARGET):' + mfdir\
		   + ' mftrace --pfa --simplify --keep-trying%(verbose)s'\
		   + ' --include=$$(dirname $TARGET)'\
		   + ' %(encoding)s %(mf)s)') % vars ()
	return os.system (command)

pfa = Builder (action = run_mftrace, suffix = '.pfa', src_suffix = '.mf',
	       emitter = add_enc_src)
env.Append (BUILDERS = {'PFA': pfa})


