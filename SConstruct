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
opts.Add ('prefix', 'Install prefix', '/usr/')
opts.Add ('outdir', 'Output directory', 'out')
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

if conf.CheckLib ('dl'):
	pass

if conf.CheckLib ('kpathsea'):
	defines['KPATHSEA'] = '1'

# ugh?
config = open ('config.h', 'w')
sort_helper = defines.keys ()
sort_helper.sort ()
#for i in defines.keys ():
for i in sort_helper:
	config.write ('#define %s %s\n' % (i[1:], defines[i]))
config.close ()
env = conf.Finish ()

os.system (sys.executable \
	   + ' ./stepmake/bin/make-version.py VERSION > version.hh')

Export ('env')

#this could happen after flower...
env.ParseConfig ('guile-config compile')

builddir = ''
outdir = env['outdir']

if os.path.exists ('parser'):
	env.Append (LIBPATH = ['#/flower', '#/lily', '#/parser', '#/gui',],
		    CPPPATH = ['#',])
else:	
	env.Append (LIBPATH = ['#/flower/' + outdir,],
		    CPPPATH = ['#',])


#ugh: remove make config output
if os.path.exists ('lily/out/config.h'):
	os.unlink ('lily/out/config.h')

subdirs = ('flower', 'lily',)
#subdirs = ('flower', 'lily', 'parser', 'gui', 'main',)
for d in subdirs:
	alias = os.path.join (builddir, d, outdir)
	env.BuildDir (alias, d)
	SConscript (os.path.join (alias, 'SConscript'))

