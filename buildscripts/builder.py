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

MAKEINFO_INCLUDES = join_path (env['MAKEINFO_PATH'], '', ' -I')
a = env['MAKEINFO'] + verbose_opt (env, ' --verbose') \
    + MAKEINFO_INCLUDES \
    + ' --no-split --no-headers --output=$TARGET $SOURCE'

texi2txt = Builder (action = a, suffix = '.txt', src_suffix = '.texi')

env.Append (BUILDERS = {'Texi2txt': texi2txt})


PYTHON = env['PYTHON']
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

## + ' --output=$$(basename $TARGET) --format=%(LILYPOND_BOOK_FORMAT)s\
##a = (r'''echo "NOT RM $$(grep -LF '\lilypondend' $$(dirname $TARGET)/lily-*.tex)"; ''' \
a = (r'''rm -f $$(grep -LF '\lilypondend' $$(dirname $TARGET)/lily-*.tex 2>/dev/null); ''' \
     + 'LILYPONDPREFIX=%(LILYPONDPREFIX)s '\
     + PYTHON + ' ' + LILYPOND_BOOK + verbose_opt (env, ' --verbose')\
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
