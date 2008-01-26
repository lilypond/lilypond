# -*-python-*-

'''
Experimental scons (www.scons.org) building.

Usage

    scons TARGET

build from source directory ./TARGET (not recursive)

Configure, build

    scons [config]         # configure
    scons              # build all

Run from build tree

    run=$(pwd)/out-scons/usr
    export LOCALE=$run/share/locale
    export TEXMF='{'$run/share/lilypond,$(kpsexpand '$TEXMF')'}'
    PATH=$run/bin:$PATH

    #optionally, if you do not use custom.py below
    #export LILYPOND_DATADIR=$run/share/lilypond/<VERSION>

    lilypond input/simple

Other targets
    scons mf-essential     # build minimal mf stuff

    scons doc          # build web doc
    scons config           # reconfigure
    scons install          # install
    scons -c           # clean
    scons -h           # help

    scons /            # build *everything* (including installation)

Options  (see scons -h)
    scons build=DIR        # clean srcdir build, output below DIR
    scons out=DIR          # write output for alterative config to DIR

Debugging
    scons --debug=dtree
    scons --debug=explain
    scons verbose=1

Optional custom.py

import os
out='out-scons'
optimising=0
debugging=1
gui=1
os.path.join (os.getcwd (), '=install')
prefix=os.path.join (os.environ['HOME'], 'usr', 'pkg', 'lilypond')

'''


# TODO:

#  * reality check:
#     - too many stages in Environments setup
#       (see also buildscripts/builders.py)
#     - Home-brew scons.cach configuration caching
#     - Home-brew source tarball generating -- [why] isn't that in SCons?

#  * usability and documentation for "./configure; make" users

#  * too much cruft in toplevel SConstruct

#  * (optional) operation without CVS directories, from tarball

#  * more program configure tests, actually use full executable name

#  * install doc

#  * split doc target: doc input examples mutopia?

#  * grep FIXME $(find . -name 'S*t')

#  * drop "fast"

import re
import glob
import os
import string
import sys
import stat
import shutil

# duh, we need 0.95.1
EnsureSConsVersion (0, 96, 92)

usage = r'''Usage:
[ENVVAR=VALUE]... scons [OPTION=VALUE]... [TARGET|DIR]...

TARGETS: clean, config, doc, dist, install, mf-essential, po-update,
     realclean, release, sconsclean, tar, TAGS

ENVVARS: BASH, CCFLAGS, CC, CXX, LIBS, PYTHON, SH...
     (see SConstruct:config_vars)

OPTIONS:
'''
      

config_cache = 'scons.cache'
if os.path.exists (config_cache) and 'config' in COMMAND_LINE_TARGETS:
    os.unlink (config_cache)

# All config_vars can be set as ENVVAR, eg:
#
#    CXX=g++-4.0 GS=~/usr/pkg/gs/bin/gs scons config
#
# append test_program variables automagically?
config_vars = [
    'BASH',
    'BYTEORDER',
    'CC',
    'CCFLAGS',
    'CPPPATH',
    'CPPDEFINES',
    'CXX',
    'CXXFLAGS',
    'DEFINES',
    'DVIPS',
    'FONTFORGE',
    'GCC',
    'GXX',
    'GS',
    'LIBS',
    'LINKFLAGS',
    'MF',
    'PERL',
    'PYTHON',
    'SH',
    ]

# Put your favourite stuff in custom.py
opts = Options ([config_cache, 'custom.py'], ARGUMENTS)
opts.Add ('prefix', 'Install prefix', '/usr/')
opts.Add ('out', 'Output directory', 'out-scons')
opts.Add ('build', 'Build directory', '.')
opts.Add ('DESTDIR', 'DESTDIR prepended to prefix', '')
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
            0),
    BoolOption ('verbose', 'run commands with verbose flag',
            0),
    BoolOption ('checksums', 'use checksums instead of timestamps',
            0),
    BoolOption ('fast', 'use timestamps, implicit cache, prune CPPPATH',
            0),
    )

srcdir = Dir ('.').srcnode ().abspath
#ugh
sys.path.append (os.path.join (srcdir, 'stepmake', 'bin'))

try:
    import packagepython
    packagepython.Package (srcdir)
    packagepython.version_tuple_to_str (package.version)
except:
    print '*** FIXME: no packagepython.  setting version to 1.0'
    class Package:
        name = 'lilypond'
        release_dir = '.'
    package = Package
    version = '1.0'

ENV = { 'PYTHONPATH': '' }
for key in ['GUILE_LOAD_PATH', 'LD_LIBRARY_PATH', 'PATH', 'PKG_CONFIG_PATH',
            'PYTHONPATH', 'TEXMF']:
    if os.environ.has_key (key):
        ENV[key] = os.environ[key]

ENV['PYTHONPATH'] = os.path.join (srcdir, 'python') + ':' + ENV['PYTHONPATH']

env = Environment (
    ENV = ENV,
    BYTEORDER = sys.byteorder.upper (),
    CC = '$GCC',
    CXX = '$GXX',
    CPPDEFINES = '-DHAVE_CONFIG_H',
    MAKEINFO = 'LANG= makeinfo',
    MF_TO_TABLE_PY = srcdir + '/buildscripts/mf-to-table.py',
    
    PKG_CONFIG_PATH = [os.path.join (os.environ['HOME'],
                     'usr/pkg/gnome/lib'),
               os.path.join (os.environ['HOME'],
                     'usr/pkg/pango/lib')],
    GZIP='-9v',
    MFMODE = 'ljfour',
    TOPLEVEL_VERSION = version,
    )

Help (usage + opts.GenerateHelpText (env))

# Add all config_vars to opts, so that they will be read and saved
# together with the other configure options.
map (lambda x: opts.AddOptions ((x,)), config_vars)
opts.Update (env)

for key in config_vars:
    if os.environ.has_key (key):
        env[key] = os.environ[key]

if env['fast']:
    # Usability switch (Anthony Roach).
    # See http://www.scons.org/cgi-bin/wiki/GoFastButton
    # First do: scons realclean .
    env['checksums'] = 0
    SetOption ('max_drift', 1)
    SetOption ('implicit_cache', 1)
elif env['checksums']:
    # Always use checksums (makes more sense than timestamps).
    SetOption ('max_drift', 0)
    # Using *content* checksums prevents rebuilds after
    # [re]configure if config.hh has not changed.  Too bad that it
    # is unusably slow.
    TargetSignatures ('content')

absbuild = Dir (env['build']).abspath
outdir = os.path.join (Dir (env['build']).abspath, env['out'])
run_prefix = os.path.join (absbuild, os.path.join (env['out'], 'usr'))


config_hh = os.path.join (outdir, 'config.hh')
version_hh = os.path.join (outdir, 'version.hh')

env.Alias ('config', config_cache)

cachedir = os.path.join (outdir, 'build-cache')

if not os.path.exists (cachedir):
    os.makedirs (cachedir)

CacheDir (cachedir)

# No need to set $LILYPOND_DATADIR to run lily, but cannot install...
if env['debugging'] and not 'install' in COMMAND_LINE_TARGETS:
    env['prefix'] = run_prefix

prefix = env['prefix']
bindir = os.path.join (prefix, 'bin')
sharedir = os.path.join (prefix, 'share')
libdir = os.path.join (prefix, 'lib')
libdir_package = os.path.join (libdir, package.name)
libdir_package_version = os.path.join (libdir_package, version)
localedir = os.path.join (sharedir, 'locale')
sharedir_doc_package = os.path.join (sharedir, 'doc', package.name)
sharedir_package = os.path.join (sharedir, package.name)
sharedir_package_version = os.path.join (sharedir_package, version)
lilypondprefix = sharedir_package_version

# junkme
env.Append (
    absbuild = absbuild,
    srcdir = srcdir,
    )



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
            if src.find ('@') > -1:
                frm = os.path.join ('../' * depth,
                            string.replace (src, '@',
                                    env['out']))
            else:
                frm = os.path.join ('../' * depth, src,
                            env['out'])
        if src[-1] == '/':
            frm = os.path.join (frm, os.path.basename (dst))
        if env['verbose']:
            print 'ln -s %s -> %s' % (frm, os.path.basename (dst))
        os.symlink (frm, os.path.basename (dst))
    shutil.rmtree (run_prefix)
    prefix = os.path.join (env['out'], 'usr')
    map (lambda x: symlink (x[0], os.path.join (prefix,
                            x[1] % {'ver' : version})),
         # ^# := source dir
         # @  := out
         # /$ := add dst file_name
         (('python',     'lib/lilypond/python'),
          # ugh
          ('python',     'share/lilypond/%(ver)s/python'),
          ('lily/',      'bin/lilypond'),
          ('scripts/',   'bin/convert-ly'),
          ('scripts/',   'bin/lilypond-book'),
          ('scripts/',   'bin/ps2png'),
          ('mf',     'share/lilypond/%(ver)s/dvips/mf-out'),
          ('#ps/music-drawing-routines.ps',
           'share/lilypond/%(ver)s/tex/music-drawing-routines.ps'),
          ('mf',     'share/lilypond/%(ver)s/otf'),
          ('mf',     'share/lilypond/%(ver)s/tfm'),
          ('tex',    'share/lilypond/%(ver)s/tex/enc'),
          ('#mf',    'share/lilypond/%(ver)s/fonts/mf'),
          ('mf',     'share/lilypond/%(ver)s/fonts/map'),
          ('mf',     'share/lilypond/%(ver)s/fonts/otf'),
          ('mf',     'share/lilypond/%(ver)s/fonts/tfm'),
          ('mf',     'share/lilypond/%(ver)s/fonts/type1'),
          ('#tex',       'share/lilypond/%(ver)s/tex/source'),
          ('tex',    'share/lilypond/%(ver)s/tex/tex-out'),
          ('mf',     'share/lilypond/%(ver)s/tex/mf-out'),
          ('#ly',    'share/lilypond/%(ver)s/ly'),
          ('#scm',       'share/lilypond/%(ver)s/scm'),
          ('#scripts',   'share/lilypond/%(ver)s/scripts'),
          ('#ps',    'share/lilypond/%(ver)s/ps'),
          ('po/@/nl.mo', 'share/locale/nl/LC_MESSAGES/lilypond.mo'),
          ('elisp',      'share/lilypond/%(ver)s/elisp')))

    print "FIXME: BARF BARF BARF"
    os.chdir (absbuild)
    out = env['out']
    ver = version
    prefix = os.path.join (env['out'], 'usr/share/lilypond/%(ver)s/fonts'
                   % vars ())
    for ext in ('enc', 'map', 'otf', 'svg', 'tfm', 'pfa'):
        dir = os.path.join (absbuild, prefix, ext)
        os.system ('rm -f ' + dir)
        mkdirs (dir)
        os.chdir (dir)
        os.system ('ln -s ../../../../../../../mf/%(out)s/*.%(ext)s .'
               % vars ())
    os.chdir (srcdir)

def configure (target, source, env):
    dre = re.compile ('\n(200[0-9]{5})')
    vre = re.compile ('.*?\n[^-.0-9]*([0-9][0-9]*\.[0-9]([.0-9]*[0-9])*)',
              re.DOTALL)
    def get_version (program):
        command = '(pkg-config --modversion %(program)s || %(program)s --version || %(program)s -V) 2>&1' % vars ()
        pipe = os.popen (command)
        output = pipe.read ()
        if pipe.close ():
            return None
        splits = re.sub ('^|\s', '\n', output)
        date_hack = re.sub (dre, '\n0.0.\\1', splits)
        m = re.match (vre, date_hack)
        v = m.group (1)
        if v[-1] == '\n':
            v = v[:-1]
        return string.split (v, '.')

    def test_version (lst, full_name, minimal, description, package):
        program = os.path.basename (full_name)
        sys.stdout.write ('Checking %s version... ' % program)
        actual = get_version (program)
        if not actual:
            print 'not found'
            lst.append ((description, package, minimal, program,
                     'not installed'))
            return 0
        print string.join (actual, '.')
        if map (string.atoi, actual) \
           < map (string.atoi, string.split (minimal, '.')):
            lst.append ((description, package, minimal, program,
                     string.join (actual, '.')))
            return 0
        return 1

    def test_program (lst, program, minimal, description, package):
        key = program.upper ()
        if key.find ('+-'):
            key = re.sub ('\+', 'X', key)
            key = re.sub ('-', '_', key)
        sys.stdout.write ('Checking for %s ... ' % program)
        if env.has_key (key):
            f = env[key]
            sys.stdout.write ('(cached) ')
        else:
            f = WhereIs (program)
            env[key] = f
        if not f:
            print 'not found'
            lst.append ((description, package, minimal, program,
                     'not installed'))
            return 0
        print f
        return test_version (lst, program, minimal, description, package)

    def test_lib (lst, program, minimal, description, package):
        # FIXME: test for Debian or RPM (or -foo?) based dists
        # to guess (or get correct!: apt-cache search?)
        # package name.
        #if os.system ('pkg-config --atleast-version=0 freetype2'):
        # barf
        if test_version (lst, program, minimal, description,
                 'lib%(package)s-dev or %(package)s-devel'
                 % vars ()):
            env.ParseConfig ('pkg-config --cflags --libs %(program)s'
                     % vars ())
            return 1
        return 0

    required = []
    test_program (required, 'bash', '2.0', 'Bash', 'bash')
    test_program (required, 'gcc', '4.0', 'GNU C compiler', 'gcc')
    test_program (required, 'g++', '4.0.5', 'GNU C++ compiler', 'g++')
    test_program (required, 'guile-config', '1.8', 'GUILE development',
            'libguile-dev or guile-devel')
    test_program (required, 'mf', '0.0', 'Metafont', 'tetex-bin')
    test_program (required, 'python', '2.1', 'Python (www.python.org)',
              'python')
    # Silly, and breaks with /bin/sh == dash
    #test_program (required, 'sh', '0.0', 'Bourne shell', 'sh')

    optional = []
    # Do not use bison 1.50 and 1.75.
    #test_program (optional, 'foo', '2.0', 'Foomatic tester', 'bar')
    test_program (optional, 'bison', '1.25', 'Bison -- parser generator',
            'bison')
    test_program (optional, 'fontforge', '0.0.20050624', 'FontForge',
              'fontforge')
    test_program (optional, 'flex', '0.0', 'Flex -- lexer generator',
              'flex')
    test_program (optional, 'guile', '1.8', 'GUILE scheme', 'guile')
    test_program (optional, 'gs', '8.15',
              'Ghostscript PostScript interpreter',
              'gs or gs-afpl or gs-esp or gs-gpl')
    test_program (optional, 'makeinfo', '4.8', 'Makeinfo tool', 'texinfo')
    test_program (optional, 'perl', '4.0',
              'Perl practical efficient readonly language', 'perl')

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
       'DIRSEP' : "'%s'" % os.sep,
       'PATHSEP' : "'%s'" % os.pathsep,
       'PACKAGE': '"%s"' % package.name,
       'DATADIR' : '"%s"' % sharedir,
       'PACKAGE_DATADIR' : '"%s"' % sharedir_package,
       'LOCALEDIR' : '"%s"' %localedir,
    }
    conf.env.Append (DEFINES = defines)

    command = r"""python -c 'import sys; sys.stdout.write ("%s/include/python%s" % (sys.prefix, sys.version[:3]))'""" #"
    PYTHON_INCLUDE = os.popen (command).read ()#[:-1]
    if env['fast']:
        env.Append (CCFLAGS = ['-I%s' % PYTHON_INCLUDE])
    else:
        env.Append (CPPPATH = [PYTHON_INCLUDE])

    headers = ('assert.h', 'grp.h', 'libio.h', 'pwd.h',
           'sys/stat.h', 'utf8/wchar.h', 'wchar.h', 'Python.h')
    for i in headers:
        if conf.CheckCHeader (i):
            key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
            conf.env['DEFINES'][key] = 1

    ccheaders = ('sstream',)
    for i in ccheaders:
        if conf.CheckCXXHeader (i):
            key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
            conf.env['DEFINES'][key] = 1

    functions = ('chroot', 'fopencookie', 'funopen',
             'gettext', 'isinf',
             'mbrtowc', 'memmem', 'snprintf', 'vsnprintf', 'wcrtomb')
    for i in functions:
        if 0 or conf.CheckFunc (i):
            key = re.sub ('[./]', '_', 'HAVE_' + string.upper (i))
            conf.env['DEFINES'][key] = 1

    if conf.CheckYYCurrentBuffer ():
        conf.env['DEFINES']['HAVE_FLEXLEXER_YY_CURRENT_BUFFER'] = 1

    if conf.CheckLib ('dl'):
        pass

    if env['fast']:
        cpppath = []
        if env.has_key ('CPPPATH'):
            cpppath = env['CPPPATH']

    ## FIXME: linkage, check for libguile.h and scm_boot_guile
    #this could happen after flower...
    env.ParseConfig ('guile-config compile')

    test_program (required, 'pkg-config', '0.9.0',
              'pkg-config library compile manager', 'pkg-config')
    if test_lib (required, 'freetype2', '0.0',
             'Development files for FreeType 2 font engine',
             'freetype6'):
        conf.env['DEFINES']['HAVE_FREETYPE2'] = '1'
        
    if test_lib (required, 'pangoft2', '1.6.0',
             'Development files for pango, with FreeType2',
             'pango1.0'):
        conf.env['DEFINES']['HAVE_PANGO_FT2'] = '1'

    if test_lib (optional, 'fontconfig', '2.2.0',
             'Development files for fontconfig', 'fontconfig1'):
        conf.env['DEFINES']['HAVE_FONTCONFIG'] = '1'
    
    #this could happen only for compiling pango-*
    if env['gui']:
        test_lib (required, 'gtk+-2.0', '2.4.0',
              'Development files for GTK+', 'gtk2.0')
            
    if env['fast']:
        # Using CCFLAGS = -I<system-dir> rather than CPPPATH = [
        # <system-dir>] speeds up SCons
        env['CCFLAGS'] += map (lambda x: '-I' + x,
                       env['CPPPATH'][len (cpppath):])
        env['CPPPATH'] = cpppath

    if required:
        print
        print '********************************'
        print 'Please install required packages'
        for i in required:
            print '%s:      %s-%s or newer (found: %s %s)' % i
        Exit (1)

    if optional:
        print
        print '*************************************'
        print 'Consider installing optional packages'
        for i in optional:
            print '%s:      %s-%s or newer (found: %s %s)' % i

    return conf.Finish ()

def config_header (target, source, env):
    config = open (str (target[0]), 'w')
    for i in sorted (env['DEFINES'].keys ()):
        config.write ('#define %s %s\n' % (i, env['DEFINES'][i]))
    config.close ()
env.Command (config_hh, config_cache, config_header)

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

def uniquify_config_vars (env):
    for i in config_vars:
        if env.has_key (i) and type (env[i]) == type ([]):
            env[i] = uniquify (env[i])

def save_config_cache (env):
    ## FIXME: Is this smart, using option cache for saving
    ## config.cache?  I cannot seem to find the official method.
    uniquify_config_vars (env)
    opts.Save (config_cache, env)

    if 'config' in COMMAND_LINE_TARGETS:
        sys.stdout.write ('\n')
        sys.stdout.write ('LilyPond configured')
        sys.stdout.write ('\n')
        sys.stdout.write ('Now run')
        sys.stdout.write ('\n')
        sys.stdout.write ('    scons [TARGET|DIR]...')
        sys.stdout.write ('\n')
        sys.stdout.write ('\n')
        sys.stdout.write ('Examples:')
        sys.stdout.write ('\n')
        sys.stdout.write ('    scons lily    # build lilypond')
        sys.stdout.write ('\n')
        sys.stdout.write ('    scons all     # build everything')
        sys.stdout.write ('\n')
        sys.stdout.write ('    scons doc     # build documentation')
        sys.stdout.write ('\n')
        ## TODO
        ## sys.stdout.write ('    scons prefix=/usr DESTDIR=/tmp/pkg all install')
        ## sys.stdout.write ('\n')
        Exit (0)
    elif not env['checksums']:
        # When using timestams, config.hh is NEW.  The next
        # build triggers recompilation of everything.  Exiting
        # here makes SCons use the actual timestamp for config.hh
        # and prevents recompiling everything the next run.
        command = sys.argv[0] + ' ' + string.join (COMMAND_LINE_TARGETS)
        sys.stdout.write ('Running %s ... ' % command)
        sys.stdout.write ('\n')
        s = os.system (command)
        Exit (s)

# WTF?
# scons: *** Calling Configure from Builders is not supported.
# env.Command (config_cache, None, configure)
if not os.path.exists (config_cache) \
   or (os.stat ('SConstruct')[stat.ST_MTIME]
       > os.stat (config_cache)[stat.ST_MTIME]):
    env = configure (None, None, env)
    save_config_cache (env)
elif env['checksums']:
    # just save everything
    save_config_cache (env)

#urg how does #/ subst work?
Export ('env')
SConscript ('buildscripts/builder.py')

env.PrependENVPath ('PATH',
            os.path.join (env['absbuild'], env['out'], 'usr/bin'))

LILYPOND_DATADIR = os.path.join (run_prefix, 'share/lilypond/', version)

if not os.path.exists (LILYPOND_DATADIR):
    os.makedirs (LILYPOND_DATADIR)

env.Command (LILYPOND_DATADIR, ['#/SConstruct', '#/VERSION'], symlink_tree)
env.Depends ('lily', LILYPOND_DATADIR)

env.Append (ENV = {
    'LILYPOND_DATADIR' : LILYPOND_DATADIR,
    'TEXMF' : '{$LILYPOND_DATADIR,'
    + os.popen ('kpsexpand \$TEXMF').read ()[:-1] + '}',
    })

BUILD_ABC2LY = '${set__x}$PYTHON $srcdir/scripts/abc2ly.py'
BUILD_LILYPOND = '$absbuild/lily/$out/lilypond ${__verbose}'
BUILD_LILYPOND_BOOK = '$PYTHON $srcdir/scripts/lilypond-book.py ${__verbose}'

if env['verbose'] and env['verbose'] != '0':
    env['__verbose'] = ' --verbose'
    env['set__x'] = 'set -x;'

# post-option environment-update
env.Append (
    bindir = bindir,
    sharedir = sharedir,
    lilypond_datadir = sharedir_package,
    localedir = localedir,
    local_lilypond_datadir = sharedir_package_version,
    lilypondprefix = lilypondprefix,
    sharedir_package = sharedir_package,
    sharedir_doc_package = sharedir_doc_package,
    sharedir_package_version = sharedir_package_version,
    libdir_package = libdir_package,
    libdir_package_version = libdir_package_version,

    LILYPOND = BUILD_LILYPOND,
    ABC2LY = BUILD_ABC2LY,
    LILYPOND_BOOK = BUILD_LILYPOND_BOOK,
    LILYPOND_BOOK_FORMAT = 'texi-html',
    MAKEINFO_FLAGS = '--css-include=$srcdir/Documentation/texinfo.css',
    )

env.Append (CCFLAGS = ['-pipe', '-Wno-pmf-conversions'])
if env['debugging']:
    env.Append (CCFLAGS = ['-g'])
if env['optimising']:
    env.Append (CCFLAGS = '-O2')
if env['warnings']:
    env.Append (CCFLAGS = ['-W', '-Wall'])
    env.Append (CXXFLAGS = ['-Wconversion'])

# ugr,huh?
env.Append (LINKFLAGS = ['-Wl,--export-dynamic'])
# FIXME: ParseConfig ignores -L flag?
env.Append (LINKFLAGS = ['-L/usr/X11R6/lib'])

## Explicit target and dependencies

if 'clean' in COMMAND_LINE_TARGETS:
    # ugh: prevent reconfigure instead of clean
    os.system ('touch %s' % config_cache)
    
    command = sys.argv[0] + ' -c .'
    sys.stdout.write ('Running %s ... ' % command)
    sys.stdout.write ('\n')
    s = os.system (command)
    if os.path.exists (config_cache):
        os.unlink (config_cache)
    Exit (s)

if 'sconsclean' in COMMAND_LINE_TARGETS:
    command = 'rm -rf scons.cache $(find . -name ".scon*")'
    s = os.system (command)
    if os.path.exists (config_cache):
        os.unlink (config_cache)
    Exit (s)
    
if 'realclean' in COMMAND_LINE_TARGETS:
    command = 'rm -rf $(find . -name "out-scons" -o -name ".scon*")'
    sys.stdout.write ('Running %s ... ' % command)
    sys.stdout.write ('\n')
    s = os.system (command)
    if os.path.exists (config_cache):
        os.unlink (config_cache)
    Exit (s)

# Declare SConscript phonies 
env.Alias ('minimal', config_cache)

if 0:
    env.Alias ('mf-essential', config_cache)
    env.Alias ('minimal', ['python', 'lily', 'mf-essential'])
    env.Alias ('all', ['minimal', 'mf', '.'])

else:
    env.Alias ('minimal', ['python', 'lily', 'mf'])
    env.Alias ('all', ['minimal', '.'])


# Do we want the doc/web separation?
env.Alias ('doc',
       ['minimal',
        'Documentation',
        'Documentation/user',
        'Documentation/topdocs',
        'Documentation/bibliography',
        'input'])

# Without target arguments, do minimal build
if not COMMAND_LINE_TARGETS:
    env.Default (['minimal'])

# GNU Make rerouting compat:
env.Alias ('web', 'doc')


env.Command (version_hh, '#/VERSION',
         '$PYTHON ./stepmake/bin/make-version.py VERSION > $TARGET')

# post-config environment update
env.Append (
    run_prefix = run_prefix,
    LILYPOND_DATADIR = LILYPOND_DATADIR,

    # FIXME: move to lily/SConscript?
    LIBPATH = [os.path.join (absbuild, 'flower', env['out'])],
    CPPPATH = [outdir, ],
    LILYPOND_PATH = ['.',
             '$srcdir/input',
             '$srcdir/input/regression',
             '$srcdir/input/test',
             '$srcdir/input/tutorial',
             '$srcdir/Documentation/user',
             '$absbuild/mf/$out',
#            os.path.join (absbuild, 'Documentation',
#                      env['out']),
#            os.path.join (absbuild, 'Documentation/user',
#                      env['out']),
             ],
    MAKEINFO_PATH = ['.', '$srcdir/Documentation/user',
             '$absbuild/Documentation/user/$out'],
    )

#### dist, tar
def plus (a, b):
    a + b

def cvs_entry_is_dir (line):
    return line[0] == 'D' and line[-2] == '/'

def cvs_entry_is_file (line):
    return line[0] == '/' and line[-2] == '/'

def cvs_dirs (dir):
    entries = os.path.join (dir, 'CVS/Entries')
    if not os.path.exists (entries):
        return []
    entries = open (entries).readlines ()
    dir_entries = filter (cvs_entry_is_dir, entries)
    dirs = map (lambda x: os.path.join (dir, x[2:x[2:].index ('/')+3]),
            dir_entries)
    return dirs + map (cvs_dirs, dirs)

def cvs_files (dir):
    entries = os.path.join (dir, 'CVS/Entries')
    if not os.path.exists (entries):
        return []
    entries = open (entries).readlines ()
    file_entries = filter (cvs_entry_is_file, entries)
    files = map (lambda x: x[1:x[1:].index ('/')+1], file_entries)
    return map (lambda x: os.path.join (dir, x), files)

def flatten (tree, lst):
    if type (tree) == type ([]):
        for i in tree:
            if type (i) == type ([]):
                flatten (i, lst)
            else:
                lst.append (i)
    return lst

if os.path.isdir ('%(srcdir)s/CVS' % vars ()):
    subdirs = flatten (cvs_dirs ('.'), [])
else:
    # ugh
    command = 'cd %(srcdir)s \
    && find . -name SConscript | sed s@/SConscript@@' % vars ()
    subdirs = string.split (os.popen (command).read ())

if env['fast']\
   and 'all' not in COMMAND_LINE_TARGETS\
   and 'doc' not in COMMAND_LINE_TARGETS\
   and 'web' not in COMMAND_LINE_TARGETS\
   and 'install' not in COMMAND_LINE_TARGETS\
   and 'clean' not in COMMAND_LINE_TARGETS:
    subdirs = [ 'python',
            'lily',
           'flower',
           'mf',
           ]

if os.path.isdir ('%(srcdir)s/CVS' % vars ()):
    src_files = reduce (lambda x, y: x + y, map (cvs_files, subdirs))
else:
    src_files = ['foobar']

readme_files = ['AUTHORS', 'README', 'INSTALL', 'NEWS']
txt_files = map (lambda x: x + '.txt', readme_files)


#
# speeds up build by +- 5% 
# 
if not env['fast']:
    foo = map (lambda x: env.TXT (x + '.txt',
                      os.path.join ('Documentation/topdocs', x)),
           readme_files)
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
               ['rm -f $$(find $TARGET.dir -name .sconsign)',
                'tar czf $TARGET -C $TARGET.dir %s' % tar_base,])
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

#### web
if not env['fast']:
    web_base = os.path.join (outdir, 'web')
    web_ball = web_base + '.tar.gz'
    env['footify'] = 'MAILADDRESS=bug-lilypond@gnu.org $PYTHON stepmake/bin/add-html-footer.py --name=lilypond --version=$TOPLEVEL_VERSION'
    web_ext = ['.html', '.ly', '.midi', '.pdf', '.png', '.ps.gz', '.txt',]
    web_path = '-path "*/$out/*"' + string.join (web_ext, ' -or -path "*/$out/*"') + '-or -type l'
    env['web_path'] = web_path
    web_list = os.path.join (outdir, 'weblist')
    # compatible make heritits
    # fixme: generate in $outdir is cwd/builddir
    env.Command (web_list,
             ## Adding 'doc' dependency is correct, but takes
             ## > 5min extra if you have a peder :-)
             #'doc',
             
             '#/VERSION',
             ['$PYTHON buildscripts/mutopia-index.py -o examples.html ./',
              'cd $absbuild && $footify $$(find . -name "*.html" -print)',
              'cd $absbuild && rm -f $$(find . -name "*.html~" -print)',
              'cd $absbuild && find Documentation input $web_path \
              > $TARGET',
              '''echo '<META HTTP-EQUIV="refresh" content="0;URL=Documentation/out-www/index.html">' > $absbuild/index.html''',
              '''echo '<html><body>Redirecting to the documentation index...</body></html>' >> $absbuild/index.html''',
              'cd $absbuild && ls *.html >> $TARGET',])
    env.Command (web_ball, web_list,
             ['cat $SOURCE | tar -C $absbuild -czf $TARGET -T -',])
    #env.Alias ('web', web_ball)
    www_base = os.path.join (outdir, 'www')
    www_ball = www_base + '.tar.gz'
    env.Command (www_ball, web_ball,
             ['rm -rf $out/tmp',
              'mkdir -p $absbuild/$out/tmp',
              'tar -C $absbuild/$out/tmp -xzf $SOURCE',
              'cd $absbuild/$out/tmp && for i in $$(find . -name "$out"); '
              + ' do mv $$i $$(dirname $$i)/out-www; done',
              'tar -C $absbuild/$out/tmp -czf $TARGET .'])
    env.Alias ('web', www_ball)

#### tags
env.Append (
    ETAGSFLAGS = """--regex='{c++}/^LY_DEFINE *(\([^,]+\)/\\1/' \
    --regex='{c++}/^LY_DEFINE *([^"]*"\([^"]+\)"/\\1/'""")
code_ext = ['.cc', '.hh', '.scm', '.tcc',]
env.Command ('TAGS', filter (lambda x: os.path.splitext (x)[1] in code_ext,
                 src_files),
         'etags $ETAGSFLAGS $SOURCES')

# Note: SConscripts are only needed in directories where something needs
# to be done, building or installing
for d in subdirs:
    if os.path.exists (os.path.join (d, 'SConscript')):
        b = os.path.join (env['build'], d, env['out'])
        # Support clean sourcetree build (--srcdir build)
        # and ./out build.
        if os.path.abspath (b) != os.path.abspath (d):
            env.BuildDir (b, d, duplicate = 0)
        SConscript (os.path.join (b, 'SConscript'))

env.Command ('tree', ['#/VERSION', '#/SConstruct'], symlink_tree)
