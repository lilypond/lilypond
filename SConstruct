# -*-python-*-
#
# Experimental scons building
# TODO:
#   * install
#   * build in out/ ?
#   * mf, Documentation, ly etc.
#   * --srcdir ?  (mkdir =build; cd =build; scons -Y .. ;
#     ===> os.chdir (Dir ('.').srcdir ()); glob.glob (*.cc); os.chdir (cwd) ?
# 

import re
import glob
import os
import sys
import string

env = Environment ()

opts = Options (None, ARGUMENTS)
##opts.Add (PackageOption ('prefix', 'Install prefix', '/usr/'))
## `Path' means a directory rather than a path?
opts.Add (PathOption ('prefix', 'Install prefix', '/usr/'))
opts.AddOptions (
	BoolOption ('warnings', 'compile with -Wall and similiar',
		   1),
	BoolOption ('debugging', 'compile with debugging symbols',
		    0),
	BoolOption ('optimising', 'compile with optimising',
		    1),
	BoolOption ('shared', 'build shared libraries',
		    1),
	BoolOption ('static', 'build static libraries',
		    0),
	)

Help (opts.GenerateHelpText (env))

env = Environment (options = opts)

if env['debugging']:
	env.Append (CFLAGS = '-g')
	env.Append (CXXFLAGS = '-g')
if env['optimising']:
	env.Append (CFLAGS = '-O2')
	env.Append (CXXFLAGS = '-O2 -DSTRING_UTILS_INLINED')
if env['warnings']:
	env.Append (CFLAGS = '-W -Wall')
	env.Append (CXXFLAGS = '-W -Wall -Wconversion')

conf = Configure (env)

#ugh -- hardcode territory
defines = {
   'DIRSEP' : "'/'",
   'PATHSEP' : "':'",

   'PACKAGE': '"lilypond"',
   'TOPLEVEL_VERSION' : '"2.3.6"',
   'DATADIR' : '"' + os.getcwd () + '/share"',
   'PACKAGE_DATADIR': 'DATADIR "/" PACKAGE',
   'LILYPOND_DATADIR' : 'PACKAGE_DATADIR',
   'LOCAL_PACKAGE_DATADIR' : 'PACKAGE_DATADIR "/" TOPLEVEL_VERSION',
   'LOCAL_LILYPOND_DATADIR' : 'LOCAL_PACKAGE_DATADIR',
   'LOCALEDIR' : '"' + os.getcwd () + '/share/locale"',
}

headers = ('sys/stat.h', 'assert.h', 'kpathsea/kpathsea.h')
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

if conf.CheckLib ('dl'):
	pass

if conf.CheckLib ('kpathsea'):
	defines['KPATHSEA'] = '1'

# ugh?
config = open ('config.h', 'w')
for i in defines.keys ():
	config.write ('#define %s %s\n' % (i, defines[i]))
config.close ()
env = conf.Finish ()

if os.path.exists ('parser'):
	env.Append (LIBPATH = ['#/flower', '#/lily', '#/parser', '#/gui',],
		    CPPPATH = ['#',])
else:	
	env.Append (LIBPATH = ['#/flower', '#/lily',],
		    CPPPATH = ['#',])

os.system (sys.executable \
	   + ' ./stepmake/bin/make-version.py VERSION > version.hh')

Export ('env')


if 1:
	#simple: build in ./flower
	SConscript ('flower/SConscript')
else:
	# moellik: build in [/tmp/build/]flower[/out]
	# werkt 'bijna', maar glob in flower/Sconscript snapt niet
	# dat-i in flower SCRDRI moet globben
	builddir = ''
	outdir = 'out'

	d = 'flower'
	alias = os.path.join (builddir, d, outdir)
	env.BuildDir (alias, d)
	SConscript (os.path.join (alias, 'SConscript'))

env.ParseConfig ('guile-config compile')

SConscript ('lily/SConscript')
if os.path.exists ('parser'):
	SConscript ('parser/SConscript')
	SConscript ('gui/SConscript')
	SConscript ('main/SConscript')
