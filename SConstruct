# -*-python-*-

'''
Experimental scons (www.scons.org) building:

Usage:
    scons
    LILYPONDPREFIX=out-scons/usr/share/lilypond lily/out-scons/lilypond-bin

    scons install
    scons -c              # clean
    scons -h              # help

    scons build=DIR       # scrdir build, write to new tree =build
    scons out=DIR         # write output to deeper dir DIR

Optionally, make a custom.py.  I have

import os
out='out-scons'
optimising=0
debugging=1
gui=1
os.path.join (os.getcwd (), '=install')
prefix=os.path.join (os.environ['HOME'], 'usr', 'pkg', 'lilypond')

'''


# TODO:
#   * separate environments?
#     - compile environment checks headers and libraries
#     - doc environment checks doc stuff
#
#   * running from build-dir, without installing?
#     - scons will not install, if PREFIX lives outside of CWD
#     - build symlink tree
#       + mimicking regular installation setup?
#       + use tweaked scons 'install' target?
#   * commandline targets:
#      - clean => -c
#      - dist, tar => env.Tar
#   * Documentation, scripts
#   * env.Tar
#   * more fine-grained config.h -- move lilypondprefix to version.hh?
#     - config.h:   changes after system upgrades, affects all files
#     - version.hh:  prefix, version etc?  affects few

import re
import glob
import os
import sys
import string

env = Environment ()

# put your favourite stuff in custom.py
opts = Options ('custom.py', ARGUMENTS)
#opts = Options (None, ARGUMENTS)
opts.Add ('prefix', 'Install prefix', '/usr/')
opts.Add ('out', 'Output directory', 'out-scons')
opts.Add ('build', 'Build directory', '.')
opts.AddOptions (
	BoolOption ('warnings', 'compile with -Wall and similiar',
		   1),
	BoolOption ('debugging', 'compile with debugging symbols',
		    0),
	BoolOption ('optimising', 'compile with optimising',
		    1),
	BoolOption ('shared', 'build shared libraries',
		    0),
	BoolOption ('static', 'build static libraries',
		    1),
	BoolOption ('gui', 'build with GNOME backend (EXPERIMENTAL)',
		    1),
	BoolOption ('verbose', 'run commands with verbose flag',
		    0),
	)

Help (opts.GenerateHelpText (env))

env = Environment (options = opts)

env.CacheDir (os.path.join (env['build'], '=build-cache'))

#ugh
sys.path.append (os.path.join ('.', 'stepmake', 'bin'))
import packagepython
package = packagepython.Package ('.')

env['version'] = packagepython.version_tuple_to_str (package.version)
env['bindir'] = os.path.join (env['prefix'], 'bin')
env['sharedir'] = os.path.join (env['prefix'], 'share')
env['libdir'] = os.path.join (env['prefix'], 'lib')
env['localedir'] = os.path.join (env['sharedir'], 'locale')

env['sharedir_package'] = os.path.join (env['sharedir'], package.name)
env['sharedir_package_version'] = os.path.join (env['sharedir_package'],
						 env['version'])
env['lilypondprefix'] = os.path.join (env['sharedir_package_version'])


if env['debugging']:
	env.Append (CFLAGS = '-g')
	env.Append (CXXFLAGS = '-g')
if env['optimising']:
	env.Append (CFLAGS = '-O2')
	env.Append (CXXFLAGS = '-O2')
	env.Append (CXXFLAGS = '-DSTRING_UTILS_INLINED')
if env['warnings']:
	env.Append (CFLAGS = '-W ')
	env.Append (CFLAGS = '-Wall')
	# what about = ['-W', '-Wall', ...]?
	env.Append (CXXFLAGS = '-W')
	env.Append (CXXFLAGS = '-Wall')
	env.Append (CXXFLAGS = '-Wconversion')

env['MFMODE'] = 'ljfour'


conf = Configure (env)


vre = re.compile ('^.*[^-.0-9]([0-9][0-9]*\.[0-9][.0-9]*).*$', re.DOTALL)
def get_version (program):
	command = '(%(program)s --version || %(program)s -V) 2>&1' % vars ()
	pipe = os.popen (command)
	output = pipe.read ()
	if pipe.close ():
		return None
	v = re.sub (vre, '\\1', output)
	return string.split (v, '.')

def assert_version (lst, program, minimal, description, package):
	global required
	sys.stdout.write ('Checking %s version... ' % program)
	actual = get_version (program)
	if not actual:
		print 'not found'
		lst.append ((description, package, minimal, program,
			     'not installed'))
		return
	sys.stdout.write (string.join (actual, '.'))
	sys.stdout.write ('\n')
	if actual < string.split (minimal, '.'):
		lst.append ((description, package, minimal, program,
			     string.join (actual, '.')))

required = []
assert_version (required, 'gcc', '2.8', 'GNU C compiler', 'gcc')
assert_version (required, 'g++', '3.0.5', 'GNU C++ compiler', 'g++')
assert_version (required, 'python', '2.1', 'Python (www.python.org)', 'python')
assert_version (required, 'guile-config', '1.6', 'GUILE development',
		'libguile-dev or guile-devel')
# Do not use bison 1.50 and 1.75.
assert_version (required, 'bison', '1.25', 'Bison -- parser generator',
		'bison')
assert_version (required, 'flex', '0.0', 'Flex -- lexer generator', 'flex')


optional = []
assert_version (optional, 'makeinfo', '4.7', 'Makeinfo tool', 'texinfo')
assert_version (optional, 'guile', '1.6', 'GUILE scheme',
		'libguile-dev or guile-devel')
assert_version (optional, 'mftrace', '1.0.27', 'Metafont tracing Type1',
		'mftrace')
assert_version (optional, 'perl', '4.0',
		'Perl practical efficient readonly language', 'perl')
#assert_version (optional, 'foo', '2.0', 'Foomatic tester', 'bar')


defines = {
   'DIRSEP' : "'/'",
   'PATHSEP' : "':'",
   'TOPLEVEL_VERSION' : '"' + env['version'] + '"',
   'PACKAGE': '"' + package.name + '"',
   'DATADIR' : '"' + env['sharedir'] + '"',
   'LILYPOND_DATADIR' : '"' + env['sharedir_package'] + '"',
   'LOCAL_LILYPOND_DATADIR' : '"' + env['sharedir_package_version'] + '"',
   'LOCALEDIR' : '"' + env['localedir'] + '"',
}


command = r"""python -c 'import sys; sys.stdout.write ("%s/include/python%s" % (sys.prefix, sys.version[:3]))'""" #"
PYTHON_INCLUDE = os.popen (command).read ()
env.Append (CPPPATH = PYTHON_INCLUDE)

headers = ('sys/stat.h', 'assert.h', 'kpathsea/kpathsea.h', 'Python.h')
for i in headers:
	if conf.CheckCHeader (i):
       		key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
                defines[key] = '1'

ccheaders = ('sstream',)
for i in ccheaders:
	if conf.CheckCXXHeader (i):
       		key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
                defines[key] = '1'

functions = ('gettext', 'isinf', 'memmem', 'snprintf', 'vsnprintf')
for i in functions:
	if 0 or conf.CheckFunc (i):
       		key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
                defines[key] = '1'

key = 'HAVE_FLEXLEXER_YY_CURRENT_BUFFER'

sys.stdout.write('Checking for yy_current_buffer ... ')
sys.stdout.flush()
res = conf.TryCompile ("""using namespace std;
#include <FlexLexer.h>
class yy_flex_lexer: public yyFlexLexer
{
  public:
    yy_flex_lexer ()
    {
      yy_current_buffer = 0;
    }
};""", '.cc')
if res:
	defines[key] = '1'
	sys.stdout.write('yes\n')
else:
	sys.stdout.write('no\n')


if conf.CheckLib ('dl'):
	pass

if conf.CheckLib ('kpathsea'):
	defines['KPATHSEA'] = '1'

# huh? 
if conf.CheckLib ('kpathsea', 'kpse_find_file'):
	defines['HAVE_KPSE_FIND_FILE'] = '1'
if conf.CheckLib ('kpathsea', 'kpse_find_tfm'):
	defines['HAVE_KPSE_FIND_TFM'] = '1'

#this could happen after flower...
env.ParseConfig ('guile-config compile')

#this could happen only for compiling pango-*
if env['gui']:
	env.ParseConfig ('pkg-config --cflags --libs gtk+-2.0')
	env.ParseConfig ('pkg-config --cflags --libs pango')
	if conf.CheckCHeader ('pango/pangofc-fontmap.h'):
		defines['HAVE_PANGO_PANGOFC_FONTMAP_H'] = '1'

	if conf.CheckLib ('pango-1.0',
			  'pango_fc_font_map_add_decoder_find_func'):
		defines['HAVE_PANGO_CVS'] = '1'
		defines['HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC'] = '1'

env = conf.Finish ()

here = os.getcwd ()
reldir = str (Dir ('.').srcnode ())
os.chdir (reldir)
srcdir = os.getcwd ()
os.chdir (here)
env['srcdir'] = srcdir

build = env['build']
out = env['out']
##reldir = str (Dir ('.').srcnode ())
reldir = os.getcwd ()
outdir = os.path.join (env['build'], reldir, env['out'])
if not os.path.exists (outdir):
	os.mkdir (outdir)

def list_sort (lst):
	sorted = lst
	sorted.sort ()
	return sorted
	
config = open (os.path.join (outdir, 'config.h'), 'w')
for i in list_sort (defines.keys ()):
	config.write ('#define %s %s\n' % (i, defines[i]))
config.close ()

os.system (sys.executable \
	   + ' ./stepmake/bin/make-version.py VERSION > '\
	   + os.path.join (outdir, 'version.hh'))

if os.path.exists ('parser'):
	env.Append (LIBPATH = ['#/flower', '#/lily', '#/parser', '#/gui',],
		    CPPPATH = [outdir, '#',])
else:	
	env.Append (LIBPATH = ['#/flower/' + out,],
		    CPPPATH = [outdir, '#',])

if required:
	print
	print '********************************'
	print 'Please install required packages'
	for i in required:
		print '%s:	%s-%s or newer (found: %s %s)' % i
	sys.exit (1)

if optional:
	print
	print '*************************************'
	print 'Consider installing optional packages'
	for i in optional:
		print '%s:	%s-%s or newer (found: %s %s)' % i

#env['tarball'] = os.path.join (outdir,
#			       package.name + '-' + env['version'] + '.tar.gz')

env['tarball'] = os.path.join (os.environ['HOME'], 'tmp',
			       package.name + '-' + env['version'] + '.tar.gz')

# huh?
if 'tar' in COMMAND_LINE_TARGETS:
	#env.Default (env['tarball'])
	#env.Default (tar)
	env.Default (env['tarball'])
	#Default (tar)

Export ('env')

#ugr
if build == '.':
	absbuild = os.getcwd ()
else:
	absbuild = build
env['absbuild'] = absbuild

# duh
env['MAKEINFO'] = 'LANG= makeinfo'
env['PYTHON'] = 'python'
env['LILYPOND_BIN'] = os.path.join (absbuild, 'lily', out, 'lilypond-bin')
env['LILYPONDPREFIX'] =	os.path.join (outdir, 'usr/share/lilypond')
env['LILYPOND_BOOK'] = srcdir + '/scripts/lilypond-book.py'
env['ABC2LY_PY'] = srcdir + '/scripts/abc2ly.py'
env['MF_TO_TABLE_PY'] = srcdir + '/buildscripts/mf-to-table.py'
env['LILYPOND_PY'] = srcdir + '/scripts/lilypond.py'
env['LILYPOND_BOOK_FLAGS'] = ''
env['LILYPOND_BOOK_FORMAT'] = 'texi-html'
# ugh?
env['LILYPOND_BOOK_PATH'] = ['.', '#/input', '#/input/regression',
			     '#/input/test', '#/input/tutorial',
			     os.path.join (absbuild, 'mf', out),
			     '#/Documentation/user',
			     os.path.join (absbuild, 'Documentation', out),
			     os.path.join (absbuild, 'Documentation/user', out),
			     ]
			     
env['MAKEINFO_PATH'] = ['.', '#/Documentation/user',
			os.path.join (absbuild, 'Documentation/user', out)]

## TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)
env['TEXINFO_PAPERSIZE_OPTION'] = '-t @afourpaper'
#FIXME: ./python isn't sconsed yet, add scrdir/python for lilylib.py ...
env.Append (PYTHONPATH = [os.path.join (outdir, 'usr/lib/python'),
			  os.path.join (srcdir, 'buildscripts'),
			  os.path.join (srcdir, 'python')])
# huh, aha?
env.Append (ENV = { 'PYTHONPATH' : string.join (env['PYTHONPATH'],
						os.pathsep) } )

# GS_FONTPATH, GS_LIB?
SConscript ('buildscripts/builder.py')

#subdirs = ['mf',]
#subdirs = ['flower', 'lily', 'parser', 'gui', 'main',]
#subdirs = ['flower', 'lily', 'mf', 'scm', 'ly']
subdirs = ['flower', 'lily', 'mf', 'scm', 'ly', 'Documentation',
	   'Documentation/user', 'input']
for d in subdirs:
	b = os.path.join (build, d, out)
	# Support clean sourctree build (srcdir build)
	# and outdir build.
	# TODO: figure out SConscript (dir, builddir, duplicate)) feature
	if (build and build != '.') \
	   or (out and out != '.'):
		env.BuildDir (b, d, duplicate=0)
	SConscript (os.path.join (b, 'SConscript'))

readme_files = ['ChangeLog', 'COPYING', 'DEDICATION', 'ROADMAP', 'THANKS']
readme_txt = ['AUTHORS.txt', 'README.txt', 'INSTALL.txt', 'NEWS.txt']
# to be [re]moved after spit
patch_files = ['emacsclient.patch', 'server.el.patch', 'darwin.patch']

map (lambda x: env.Texi2txt (x, os.path.join ('Documentation/topdocs',
					      os.path.splitext (x)[0])),
     readme_txt)

#testing
env.Append (TARFLAGS = '-z --owner=0 --group=0')
env.Append (GZIPFLAGS = '-9')
all_sources = ['SConstruct',] + subdirs \
	      + ['VERSION', '.cvsignore']\
	      + readme_files + readme_txt + patch_files

tar = env.Tar (env['tarball'], all_sources)

# as a builder?
def symlink_tree (prefix):
	def mkdirs (dir):
		def mkdir (dir):
			if not dir:
				os.chdir (os.sep)
				return
			if not os.path.isdir (dir):
				if os.path.exists (dir):
					os.unlink (dir)
				os.mkdir (dir)
			os.chdir (dir)
		map (mkdir, string.split (dir, os.sep))
	#srcdir = os.getcwd ()
	def symlink (src, dst):
		dir = os.path.dirname (dst)
		mkdirs (dir)
		if src[0] == '#':
			frm = os.path.join (srcdir, src[1:])
		else:
			depth = len (string.split (dir))
			frm = os.path.join ('../' * depth, src, out)
		os.symlink (frm, os.path.basename (dst))
		os.chdir (srcdir)
	map (lambda x: symlink (x[0], os.path.join (prefix, x[1])),
	     (('python', 'lib/lilypond/python'),
	      ('#mf',    'share/lilypond/fonts/mf'),
	      ('mf',     'share/lilypond/fonts/amf'),
	      ('mf',     'share/lilypond/fonts/tfm'),
	      ('mf',     'share/lilypond/fonts/type1'),
	      ('#tex',   'share/lilypond/tex/source'),
	      ('mf',     'share/lilypond/tex/generate'),
	      ('#ly',    'share/lilypond/ly'),
	      ('#scm',   'share/lilypond/scm'),
	      ('#ps',    'share/lilypond/ps'),
	      ('elisp',  'share/lilypond/elisp')))

if env['debugging']:
	prefix = os.path.join (outdir, 'usr')
	if not os.path.exists (prefix):
		symlink_tree (prefix)
