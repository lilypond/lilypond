# -*-python-*-

'''
Experimental scons (www.scons.org) building:

Usage:
    scons
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
prefix=os.getcwd ()


'''


# TODO:
#   * mf: pfa
#   *, Documentation, ly etc.


import re
import glob
import os
import sys
import string

env = Environment ()

# put your favourite stuff in custom.py
opts = Options ('custom.py', ARGUMENTS)
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
env['lilypondprefix'] = os.path.join (env['sharedir'], 'lilypond',
				      env['version'])

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

#ugh -- hardcode territory
defines = {
   '0DIRSEP' : "'/'",
   '1PATHSEP' : "':'",

   '2PACKAGE': '"lilypond"',
   '3TOPLEVEL_VERSION' : '"2.3.6"',
   '4DATADIR' : '"' + os.getcwd () + '/share"',
   '5PACKAGE_DATADIR': 'DATADIR "/" PACKAGE',
   '6LILYPOND_DATADIR' : 'PACKAGE_DATADIR',
   '7LOCAL_PACKAGE_DATADIR' : 'PACKAGE_DATADIR "/" TOPLEVEL_VERSION',
   '8LOCAL_LILYPOND_DATADIR' : 'LOCAL_PACKAGE_DATADIR',
   '9LOCALEDIR' : '"' + os.getcwd () + '/share/locale"',
}

headers = ('sys/stat.h', 'assert.h', 'kpathsea/kpathsea.h')
for i in headers:
	if conf.CheckCHeader (i):
       		key = re.sub ('[./]', '_', 'zHAVE_' + string.upper (i))
                defines[key] = '1'

ccheaders = ('sstream',)
for i in ccheaders:
	if conf.CheckCXXHeader (i):
       		key = re.sub ('[./]', '_', 'zHAVE_' + string.upper (i))
                defines[key] = '1'

functions = ('gettext', 'isinf', 'memmem', 'snprintf', 'vsnprintf')
for i in functions:
	if 0 or conf.CheckFunc (i):
       		key = re.sub ('[./]', '_', 'zHAVE_' + string.upper (i))
                defines[key] = '1'


key = 'zHAVE_FLEXLEXER_YY_CURRENT_BUFFER'
defines[key] = conf.TryCompile ("""using namespace std;
#include <FlexLexer.h>
class yy_flex_lexer: public yyFlexLexer
{
  public:
    yy_flex_lexer ()
    {
      yy_current_buffer = 0;
    }
};""", 'cc')

if conf.CheckLib ('dl'):
	pass

if conf.CheckLib ('kpathsea'):
	defines['zKPATHSEA'] = '1'

# huh? 
if conf.CheckLib ('kpathsea', 'kpse_find_file'):
	defines['zHAVE_KPSE_FIND_FILE'] = '1'

env = conf.Finish ()

Export ('env')

#this could happen after flower...
env.ParseConfig ('guile-config compile')

build = env['build']
out = env['out']
##reldir = str (Dir ('.').srcnode ())
reldir = os.getcwd ()
outdir = os.path.join (env['build'], reldir, env['out'])
if not os.path.exists (outdir):
	os.mkdir (outdir)

config = open (os.path.join (outdir, 'config.h'), 'w')
sort_helper = defines.keys ()
sort_helper.sort ()
for i in sort_helper:
	config.write ('#define %s %s\n' % (i[1:], defines[i]))
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

def get_version (program):
	command = '(%(program)s --version || %(program)s -V) 2>&1' % vars ()
	output = os.popen (command).readline ()[:-1]
	v = re.sub ('^.*[^-.0-9]([0-9][0-9]*\.[0-9][.0-9]*).*$', '\\1', output)
	return string.split (v, '.')

def assert_version (program, minimal, description, package):
	global required
	sys.stdout.write ('Checking %s version... ' % program)
	actual = get_version (program)
	sys.stdout.write (string.join (actual, '.'))
	sys.stdout.write ('\n')
	if actual < string.split (minimal, '.'):
		required.append ((description, package,
				  string.join (minimal, '.'),
				  program,
				  string.join (actual, '.')))

required = []
assert_version ('gcc', '3.0.5', 'GNU C compiler', 'gcc')
assert_version ('makeinfo', '4.7', 'Makeinfo tool', 'texinfo')

if required:
	print
	print '********************************'
	print 'Please install required packages'
for i in required:
	print '%s:	%s-%s or newer (found: %s-%s)' % i

#subdirs = ('mf',)
#subdirs = ('flower', 'lily', 'parser', 'gui', 'main',)
subdirs = ('flower', 'lily', 'mf')
for d in subdirs:
	b = os.path.join (build, d, out)
	# Support clean sourctree build (srcdir build)
	# and outdir build.
	# TODO: figure out SConscript (dir, builddir, duplicate)) feature
	if (build and build != '.') \
	   or (out and out != '.'):
		env.BuildDir (b, d, duplicate=0)
	SConscript (os.path.join (b, 'SConscript'))

