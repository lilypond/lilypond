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

a = env['MAKEINFO'] + verbose_opt (env, ' --verbose') \
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

a = (r'''rm -f $$(grep -LF '\lilypondend' $$(dirname $TARGET))/lily-*.tex 2>/dev/null;''' \
     + 'LILYPONDPREFIX=%(LILYPONDPREFIX)s '\
     + PYTHON + ' ' + LILYPOND_BOOK + verbose_opt (env, ' --verbose')\
     + ' --include=$$(dirname $TARGET) %(LILYPOND_BOOK_INCLUDES)s'\
     + r""" --process='%(LILYPOND_BIN)s %(LILYPOND_BOOK_INCLUDES)s'"""\
     + ' --output=$$(basename $TARGET) --format=%(LILYPOND_BOOK_FORMAT)s\
     %(LILYPOND_BOOK_FLAGS)s\
     $SOURCE') % vars ()

tely2pdf = Builder (action = a, suffix = '.texi', src_suffix = '.tely')

env.Append (BUILDERS = {'Tely2pdf': tely2pdf})

TEXINFO_PAPERSIZE_OPTION = env['TEXINFO_PAPERSIZE_OPTION']
a = '(cd $$(dirname $TARGET) &&\
 texi2dvi --batch %(TEXINFO_PAPERSIZE_OPTION)s $$(basename $SOURCE)' % vars ()

texi2dvi = Builder (action = a, suffix = '.dvi', src_suffix = '.texi')

env.Append (BUILDERS = {'Texi2dvi': texi2dvi})


# $(outdir)/lilypond/lilypond.html: $(outdir)/lilypond.texi 
# 	mkdir -p $(dir $@)
# 	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(builddir)/Documentation/texinfo.css --html $<
# 	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(builddir)/Documentation/texinfo.css --html --no-split --no-headers $<
# 	perl -i~ -pe 's!../lilypond-internals!lilypond-internals/!g' $(outdir)/lilypond.html
# 	rm -f $(outdir)/lilypond/*.png $(outdir)/lilypond/*.ly 
# 	-ln -f $(outdir)/*.png $(outdir)/*.ly $(outdir)/lilypond/

