# -*-python-*-

'''
Experimental scons (www.scons.org) building:

scons TARGET builds from source directory ./TARGET (not recursive)


Usage:
    scons
    scons lily            # build lily

    LILYPONDPREFIX=out-scons/usr/share/lilypond lily/out-scons/lilypond-bin
    scons doc             # build web doc

?    scons fonts           # build all font stuff (split this? )

    scons config          # reconfigure

    scons /               # builds all possible targets

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
#   * add missing dirs
#   * cleanup

#   * separate environments?
#     - compile environment checks headers and libraries
#     - doc environment checks doc stuff

#   * commandline targets:
#      - clean => -c ?
#   * more fine-grained config.h -- move lilypondprefix to version.hh?
#     - config.h:   changes after system upgrades, affects all files
#     - version.hh:  prefix, version etc?  affects few

import re
import glob
import os
import string
import sys
import stat
import shutil

# faster but scary: when changing #includes, do scons --implicit-deps-changed
# SetOption ('implicit_cache', 1)

# SConscripts are only needed in directories where something needs
# to be done, building or installing
# TODO: Documentation/*, input/*/*, vim, po
# rename Documentation/* to ./doc?

# somethin's broken wrt config.h checksums?
FOOSUMS = 1

subdirs = ['flower', 'lily', 'mf', 'scm', 'ly', 'Documentation',
	   'Documentation/user', 'Documentation/topdocs',
	   'input', 'scripts', 'elisp',
	   'buildscripts', 'cygwin', 'debian', 'po']

usage = r'''Usage:
scons [KEY=VALUE].. [TARGET]..

where TARGET is config|lily|all|fonts|doc|tar|dist|release
'''
      

config_cache = 'config.cache'

config_vars = (
	'BASH',
	'CFLAGS',
	'CPPPATH',
	'CXXFLAGS',
	'DEFINES',
	'LIBS',
	'METAFONT',
	'PERL',
	'PYTHON',
	)

# Put your favourite stuff in custom.py
CacheDir ("buildcache")
opts = Options ([config_cache, 'custom.py'], ARGUMENTS)
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
	BoolOption ('checksums', 'use checksums instead of timestamps',
		    1),
	)

srcdir = Dir ('.').srcnode ().abspath

#ugh
sys.path.append (os.path.join (srcdir, 'stepmake', 'bin'))
import packagepython
package = packagepython.Package (srcdir)
version = packagepython.version_tuple_to_str (package.version)

ENV = { 'PATH' : os.environ['PATH'] }
for key in ['LD_LIBRARY_PATH', 'GUILE_LOAD_PATH', 'PKG_CONFIG_PATH']:
    	if os.environ.has_key(key):
        	ENV[key] = os.environ[key]

env = Environment (
	ENV = ENV,

	BASH = '/bin/bash',
	MAKEINFO = 'LANG= makeinfo',
	PERL = '/usr/bin/perl',
	PYTHON = '/usr/bin/python',
	SH = '/bin/sh',
	
	ABC2LY_PY = srcdir + '/scripts/abc2ly.py',
	LILYPOND_BOOK = srcdir + '/scripts/lilypond-book.py',
	LILYPOND_BOOK_FLAGS = '',
	LILYPOND_BOOK_FORMAT = 'texi-html',
	LILYPOND_PY = srcdir + '/scripts/lilypond.py',
	MF_TO_TABLE_PY = srcdir + '/buildscripts/mf-to-table.py',
	
	PKG_CONFIG_PATH = [os.path.join (os.environ['HOME'],
					 'usr/pkg/gnome/lib'),
			   os.path.join (os.environ['HOME'],
					 'usr/pkg/pango/lib')],
	MFMODE = 'ljfour',
	TEXINFO_PAPERSIZE_OPTION = '-t @afourpaper',
	TOPLEVEL_VERSION = version,
	)

Help (usage + opts.GenerateHelpText (env))

map (lambda x: opts.AddOptions ((x,)), config_vars)
opts.Update (env)

if env['checksums']:
	SetOption ('max_drift', 0)

prefix = env['prefix']
bindir = os.path.join (prefix, 'bin')
sharedir = os.path.join (prefix, 'share')
libdir = os.path.join (prefix, 'lib')
localedir = os.path.join (sharedir, 'locale')
sharedir_package = os.path.join (sharedir, package.name)
sharedir_package_version = os.path.join (sharedir_package, version)
lilypondprefix = sharedir_package_version

# post-option environment-update
env.Append (
	srcdir = srcdir,
	
	bindir = bindir,
	sharedir = sharedir,
	lilypond_datadir = sharedir_package,
	localedir = localedir,
	local_lilypond_datadir = sharedir_package_version,
	lilypondprefix = lilypondprefix,
	sharedir_package = sharedir_package,
	sharedir_package_version = sharedir_package_version,
	)

env.CacheDir (os.path.join (env['build'], '=build-cache'))

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
if env['verbose']:
	env['__verbose'] = '--verbose'

# Hmm
#env.Append (ENV = {'PKG_CONFIG_PATH' : string.join (env['PKG_CONFIG_PATH'],
#						    os.pathsep),
#		   'LD_LIBRARY_PATH' : string.join (env['LD_LIBRARY_PATH'],
#						    os.pathsep),
#		   'GUILE_LOAD_PATH' : string.join (env['GUILE_LOAD_PATH'],
#						    os.pathsep), })

outdir = os.path.join (Dir (env['build']).abspath, env['out'])

# This is interesting, version.hh is generated automagically, just in
# time.  Is this a .h /.hh issue?  It seems to be, using config.hh (in
# flower/file-name.cc) works.  Bug report to SCons or rename to
# config.hh or both?
# config_h = os.path.join (outdir, 'config.hh')
config_h = os.path.join (outdir, 'config.h')
version_h = os.path.join (outdir, 'version.hh')

env.Alias ('config', config_cache)


## Explicit dependencies

# Without target arguments, build lily only
if not COMMAND_LINE_TARGETS:
	env.Default ('lily')
env.Alias ('all', '.')
env.Alias ('doc',
	   ['Documentation',
	    'Documentation/user',
	    'Documentation/topdocs'])

env.Depends (['lily', 'flower', 'all', '.'], config_h)
env.Depends ('doc', ['lily', 'mf'])
env.Depends ('input', ['lily', 'mf'])
env.Depends ('doc', ['lily', 'mf'])


def list_sort (lst):
	sorted = lst
	sorted.sort ()
	return sorted

def configure (target, source, env):
	vre = re.compile ('^.*[^-.0-9]([0-9][0-9]*\.[0-9][.0-9]*).*$', re.DOTALL)
	def get_version (program):
		command = '(%(program)s --version || %(program)s -V) 2>&1' % vars ()
		pipe = os.popen (command)
		output = pipe.read ()
		if pipe.close ():
			return None
		v = re.sub (vre, '\\1', output)
		return string.split (v, '.')

	def test_program (lst, program, minimal, description, package):
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
	test_program (required, 'gcc', '2.8', 'GNU C compiler', 'gcc')
	test_program (required, 'g++', '3.0.5', 'GNU C++ compiler', 'g++')
	test_program (required, 'python', '2.1', 'Python (www.python.org)', 'python')
	test_program (required, 'guile-config', '1.6', 'GUILE development',
			'libguile-dev or guile-devel')
	# Do not use bison 1.50 and 1.75.
	test_program (required, 'bison', '1.25', 'Bison -- parser generator',
			'bison')
	test_program (required, 'flex', '0.0', 'Flex -- lexer generator', 'flex')


	optional = []
	test_program (optional, 'makeinfo', '4.7', 'Makeinfo tool', 'texinfo')
	test_program (optional, 'guile', '1.6', 'GUILE scheme',
			'libguile-dev or guile-devel')
	test_program (optional, 'mftrace', '1.0.27', 'Metafont tracing Type1',
			'mftrace')
	test_program (optional, 'perl', '4.0',
			'Perl practical efficient readonly language', 'perl')
	#test_program (optional, 'foo', '2.0', 'Foomatic tester', 'bar')

	def CheckYYCurrentBuffer (context):
		context.Message ('Checking for yy_current_buffer... ')
		ret = conf.TryCompile ("""using namespace std;
		#include <FlexLexer.h>
		class yy_flex_lexer: public yyFlexLexer
		{
		public:
		yy_flex_lexer ()
		{
		yy_current_buffer = 0;
		}
		};""", '.cc')
		context.Result (ret)
		return ret

	conf = Configure (env, custom_tests = { 'CheckYYCurrentBuffer'
						: CheckYYCurrentBuffer })

	defines = {
	   'DIRSEP' : "'/'",
	   'PATHSEP' : "':'",
	   'TOPLEVEL_VERSION' : '"' + version + '"',
	   'PACKAGE': '"' + package.name + '"',
	   'DATADIR' : '"' + sharedir + '"',
	   'LILYPOND_DATADIR' : '"' + sharedir_package + '"',
	   'LOCAL_LILYPOND_DATADIR' : '"' + sharedir_package_version + '"',
	   'LOCALEDIR' : '"' + localedir + '"',
	}
	conf.env.Append (DEFINES = defines)

	command = r"""python -c 'import sys; sys.stdout.write ("%s/include/python%s" % (sys.prefix, sys.version[:3]))'""" #"
	PYTHON_INCLUDE = os.popen (command).read ()
	env.Append (CPPPATH = PYTHON_INCLUDE)

	headers = ('sys/stat.h', 'assert.h', 'kpathsea/kpathsea.h', 'Python.h')
	for i in headers:
		if conf.CheckCHeader (i):
			key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
			conf.env['DEFINES'][key] = 1

	ccheaders = ('sstream',)
	for i in ccheaders:
		if conf.CheckCXXHeader (i):
			key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
			conf.env['DEFINES'][key] = 1

	functions = ('gettext', 'isinf', 'memmem', 'snprintf', 'vsnprintf')
	for i in functions:
		if 0 or conf.CheckFunc (i):
			key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
			conf.env['DEFINES'][key] = 1

	if conf.CheckYYCurrentBuffer ():
		conf.env['DEFINES']['HAVE_FLEXLEXER_YY_CURRENT_BUFFER'] = 1

	if conf.CheckLib ('dl'):
		pass

	if conf.CheckLib ('kpathsea'):
		conf.env['DEFINES']['KPATHSEA'] = 1

	# huh? 
	if conf.CheckLib ('kpathsea', 'kpse_find_file'):
		conf.env['DEFINES']['HAVE_KPSE_FIND_FILE'] = '1'
	if conf.CheckLib ('kpathsea', 'kpse_find_tfm'):
		conf.env['DEFINES']['HAVE_KPSE_FIND_TFM'] = '1'

	#this could happen after flower...
	env.ParseConfig ('guile-config compile')

	#this could happen only for compiling pango-*
	if env['gui']:
		env.ParseConfig ('pkg-config --cflags --libs gtk+-2.0')
		env.ParseConfig ('pkg-config --cflags --libs pango')
		if conf.CheckCHeader ('pango/pangofc-fontmap.h'):
			conf.env['DEFINES']['HAVE_PANGO_PANGOFC_FONTMAP_H'] = '1'

		if conf.CheckLib ('pango-1.0',
				  'pango_fc_font_map_add_decoder_find_func'):
			conf.env['DEFINES']['HAVE_PANGO_CVS'] = '1'
			conf.env['DEFINES']['HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC'] = '1'

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

	return conf.Finish ()

def config_header (target, source, env):
	config = open (str (target[0]), 'w')
	for i in list_sort (env['DEFINES'].keys ()):
		config.write ('#define %s %s\n' % (i, env['DEFINES'][i]))
	config.close ()

if os.path.exists (config_cache) and 'config' in COMMAND_LINE_TARGETS:
	os.unlink (config_cache)
# WTF?
# scons: *** Calling Configure from Builders is not supported.
# env.Command (config_cache, None, configure)
if not os.path.exists (config_cache) \
   or (os.stat ('SConstruct')[stat.ST_MTIME]
       > os.stat (config_cache)[stat.ST_MTIME]):
	env = configure (None, None, env)

	# We 'should' save opts each time, but that makes config.h
	# always out of date, and that triggers recompiles, even when
	# using checksums?
	if FOOSUMS: #not env['checksums']:
		## FIXME: Is this smart, using option cache for saving
		## config.cache?  I cannot seem to find the official method.
		map (lambda x: opts.AddOptions ((x,)), config_vars)
		opts.Save (config_cache, env)
		env.Command (config_h, config_cache, config_header)

# hmm?
def xuniquify (lst):
	n = []
	for i in lst:
		if not i in n:
			n.append (i)
	lst = n
	return lst

def uniquify (lst):
	d = {}
	n = len (lst)
	i = 0
	while i < n:
		if not d.has_key (lst[i]):
			d[lst[i]] = 1
			i += 1
		else:
			del lst[i]
			n -= 1
	return lst

for i in config_vars:
	if env.has_key (i) and type (env[i]) == type ([]):
		env[i] = uniquify (env[i])

if not FOOSUMS: #env['checksums']:
	## FIXME: Is this smart, using option cache for saving
	## config.cache?  I cannot seem to find the official method.
	map (lambda x: opts.AddOptions ((x,)), config_vars)
	
	opts.Save (config_cache, env)
	env.Command (config_h, config_cache, config_header)

env.Command (version_h, '#/VERSION',
	     '$PYTHON ./stepmake/bin/make-version.py VERSION > $TARGET')

absbuild = Dir (env['build']).abspath
run_prefix = os.path.join (absbuild, os.path.join (env['out'], 'usr'))

# post-config environment update
env.Append (
	absbuild = absbuild,
	run_prefix = run_prefix,
	LILYPONDPREFIX = os.path.join (run_prefix, 'share/lilypond'),

	LIBPATH = [os.path.join (absbuild, 'flower', env['out']),],
	CPPPATH = [outdir, '#',],
	LILYPOND_BIN = os.path.join (absbuild, 'lily', env['out'],
				     'lilypond-bin'),
	LILYPOND_BOOK_PATH = ['.', '#/input', '#/input/regression',
			      '#/input/test', '#/input/tutorial',
			      os.path.join (absbuild, 'mf', env['out']),
			      '#/Documentation/user',
			      os.path.join (absbuild, 'Documentation',
					    env['out']),
			      os.path.join (absbuild, 'Documentation/user',
					    env['out']),
			      ],
	MAKEINFO_PATH = ['.', '#/Documentation/user',
			 os.path.join (absbuild, 'Documentation/user',
				       env['out'])],
	)

Export ('env')
SConscript ('buildscripts/builder.py')


def symlink_tree (target, source, env):
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
	def symlink (src, dst):
		os.chdir (absbuild)
		dir = os.path.dirname (dst)
		mkdirs (dir)
		if src[0] == '#':
			frm = os.path.join (srcdir, src[1:])
		else:
			depth = len (string.split (dir, '/'))
			frm = os.path.join ('../' * depth, src, env['out'])
		os.symlink (frm, os.path.basename (dst))
	shutil.rmtree (run_prefix)
	prefix = os.path.join (env['out'], 'usr')
	map (lambda x: symlink (x[0], os.path.join (prefix, x[1])),
	     (('python', 'lib/lilypond/python'),
	      # UGHR, lilypond.py uses lilypond-bin from PATH
	      ('lily',   'bin'),
	      ('#mf',    'share/lilypond/fonts/mf'),
	      ('mf',     'share/lilypond/fonts/afm'),
	      ('mf',     'share/lilypond/fonts/tfm'),
	      ('mf',     'share/lilypond/fonts/type1'),
	      ('#tex',   'share/lilypond/tex/source'),
	      ('mf',     'share/lilypond/tex/generate'),
	      ('#ly',    'share/lilypond/ly'),
	      ('#scm',   'share/lilypond/scm'),
	      ('#ps',    'share/lilypond/ps'),
	      ('elisp',  'share/lilypond/elisp')))
	os.chdir (srcdir)

if env['debugging']:
	stamp = os.path.join (run_prefix, 'stamp')
	env.Command (stamp, 'SConstruct', [symlink_tree, 'touch $TARGET'])
	env.Depends ('lily', stamp)

#### dist, tar
def plus (a, b):
	a + b

def cvs_entry_is_dir (line):
	return line[0] == 'D' and line[-2] == '/'

def cvs_entry_is_file (line):
	return line[0] == '/' and line[-2] == '/'

def cvs_dirs (dir):
	ENTRIES = os.path.join (dir, 'CVS/Entries')
	if not os.path.exists (ENTRIES):
		return []
	entries = open (ENTRIES).readlines ()
	dir_entries = filter (cvs_entry_is_dir, entries)
	dirs = map (lambda x: os.path.join (dir, x[2:x[2:].index ('/')+3]),
		    dir_entries)
	return dirs + map (cvs_dirs, dirs)

def cvs_files (dir):
	ENTRIES = os.path.join (dir, 'CVS/Entries')
	entries = open (ENTRIES).readlines ()
	file_entries = filter (cvs_entry_is_file, entries)
	files = map (lambda x: x[1:x[1:].index ('/')+1], file_entries)
	return map (lambda x: os.path.join (dir, x), files)

#subdirs = reduce (lambda x, y: x + y, cvs_dirs ('.'))
#print `subdirs`
readme_files = ['AUTHORS', 'README', 'INSTALL', 'NEWS']
foo = map (lambda x: env.TXT (x + '.txt',
			      os.path.join ('Documentation/topdocs', x)),
	   readme_files)
txt_files = map (lambda x: x + '.txt', readme_files)
src_files = reduce (lambda x, y: x + y, map (cvs_files, subdirs))
tar_base = package.name + '-' + version
tar_name = tar_base + '.tar.gz'
ball_prefix = os.path.join (outdir, tar_base)
tar_ball = os.path.join (outdir, tar_name)

dist_files = src_files + txt_files
ball_files = map (lambda x: os.path.join (ball_prefix, x), dist_files)
map (lambda x: env.Depends (tar_ball, x), ball_files)
map (lambda x: env.Command (os.path.join (ball_prefix, x), x,
			    'ln $SOURCE $TARGET'), dist_files)
tar = env.Command (tar_ball, src_files,
		   'tar czf $TARGET -C $TARGET.dir %s' % tar_base)
env.Alias ('tar', tar)

dist_ball = os.path.join (package.release_dir, tar_name)
env.Command (dist_ball, tar_ball,
	     'if [ -e $SOURCE -a -e $TARGET ]; then rm $TARGET; fi;' \
	     + 'ln $SOURCE $TARGET')
env.Depends ('dist', dist_ball)
patch_name = os.path.join (outdir, tar_base + '.diff.gz')
patch = env.PATCH (patch_name, tar_ball)
env.Depends (patch_name, dist_ball)
env.Alias ('release', patch)

for d in subdirs:
	if os.path.exists (os.path.join (d, 'SConscript')):
		b = os.path.join (env['build'], d, env['out'])
		# Support clean sourcetree build (--srcdir build)
		# and ./out build.
		if os.path.abspath (b) != os.path.abspath (d):
			env.BuildDir (b, d, duplicate = 0)
       		SConscript (os.path.join (b, 'SConscript'))
