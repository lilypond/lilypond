dnl aclocal.m4   -*-shell-script-*-
dnl WARNING WARNING WARNING
dnl do not edit! this is aclocal.m4, generated from /users/hanwen/usr/src/lilypond-stable/stepmake/aclocal.m4
dnl aclocal.m4   -*-shell-script-*-
dnl StepMake subroutines for configure.in


### mostly interal macros

# Get full path of executable ($1)
AC_DEFUN(STEPMAKE_GET_EXECUTABLE, [
    ## which doesn't work in ash, if /usr/bin/which isn't installed
    ## type -p doesn't work in ash
    ## command -v doesn't work in zsh
    ## command -v "$1" 2>&1
    ## this test should work in ash, bash, pdksh (ksh), zsh
    type -p "$1" 2>/dev/null | tail -1 | awk '{print $NF}'
])


# Get version string from executable ($1)
AC_DEFUN(STEPMAKE_GET_VERSION, [
    ## "$1" --version 2>&1 | grep -v '^$' | head -1 | awk '{print $NF}'
    ##
    ## ARG.
    ## Workaround for broken Debian gcc version string:
    ##     gcc (GCC) 3.1.1 20020606 (Debian prerelease)
    ##
    ## Assume, and hunt for, dotted version multiplet.
    changequote(<<, >>)dnl
    "$1" --version 2>&1 | grep '[0-9]\.[0-9]' | head -1 | \
	sed -e 's/.*[^-.0-9]\([0-9][0-9]*\.[0-9][.0-9]*\).*/\1/'
    changequote([, ])dnl
])

# Calculate simplistic numeric version from version string ($1)
# As yet, we have no need for something more elaborate.
AC_DEFUN(STEPMAKE_NUMERIC_VERSION, [
    echo "$1" | awk -F. '
    {
      if ([$]3) {three = [$]3}
      else {three = 0}
    }
    {printf "%d\n", [$]1*1000000 + [$]2*1000 + three}'
])


# Add item ($2) to list ($1, one of 'OPTIONAL', 'REQUIRED')
AC_DEFUN(STEPMAKE_ADD_ENTRY, [
    eval "$1"=\"`eval echo \"'$'$1\" \"$2\"`\"
])

# Check if tested program ($2) was found ($1).
# If not, add entry to missing-list ($3, one of 'OPTIONAL', 'REQUIRED').
# We could abort here if a 'REQUIRED' program is not found
AC_DEFUN(STEPMAKE_OPTIONAL_REQUIRED, [
    STEPMAKE_CHECK_SEARCH_RESULT($1)
    if test $? -ne 0; then
	STEPMAKE_ADD_ENTRY($3, $2)
	if test "$3" = "REQUIRED"; then
	    command="echo ERROR: $2 not found"
	    # abort configure process here?
	else
	    command="- echo $2 not found"
	fi
	eval "$1"='$command'
	false
    else
	true
    fi
])


# Return if tested proram ($1) was found (true) or not (false).
AC_DEFUN(STEPMAKE_CHECK_SEARCH_RESULT, [
    r="`eval echo '$'"$1"`"
    if test -n "$r" -a "$r" != "error" -a "$r" != "no" && expr '`eval echo '$'"$1"`' : '.*\(echo\)' > /dev/null; then
	true
    else
	##STEPMAKE_WARN(cannot find $2. $3)
	false
    fi
])


# Check version of program ($1)
# If version is smaller than requested ($3),
# add entry to missing-list ($2, one of 'OPTIONAL', 'REQUIRED').
AC_DEFUN(STEPMAKE_CHECK_VERSION, [
    r="`eval echo '$'"$1"`"
    AC_MSG_CHECKING("$r version")
    #exe=`STEPMAKE_GET_EXECUTABLE($r)`
    exe=`STEPMAKE_GET_EXECUTABLE($r)`
    ver=`STEPMAKE_GET_VERSION($exe)`
    num=`STEPMAKE_NUMERIC_VERSION($ver)`
    req=`STEPMAKE_NUMERIC_VERSION($3)`
    AC_MSG_RESULT("$ver")
    if test "$num" -lt "$req"; then
	STEPMAKE_ADD_ENTRY($2, "$r $3 (installed: $ver)")
    fi
])

### Macros to build configure.in


AC_DEFUN(STEPMAKE_BIBTEX2HTML, [
    STEPMAKE_PROGS(BIBTEX2HTML, bibtex2html bib2html, $1)
    if test "$BIBTEX2HTML" = "bib2html"; then
	BIBTEX2HTML_FLAGS='$< $(@)'
    else
	BIBTEX2HTML_FLAGS='-o $(@D)/$(*F) $<'
    fi
    AC_SUBST(BIBTEX2HTML)
    AC_SUBST(BIBTEX2HTML_FLAGS)
])


AC_DEFUN(STEPMAKE_BISON, [
    # ugh, automake: we want (and check for) bison
    AC_PROG_YACC
    
    STEPMAKE_PROGS(BISON, bison, $1)
    
    # urg.  should test functionality rather than version.
    if test "$BISON" = "bison" -a -n "$2"; then
	STEPMAKE_CHECK_VERSION(BISON, $1, $2)
    fi
])


AC_DEFUN(STEPMAKE_COMPILE, [
    # -O is necessary to get inlining
    CFLAGS=${CFLAGS-""}
    CXXFLAGS=${CXXFLAGS-$CFLAGS}
    LDFLAGS=${LDFLAGS-""}
    checking_b=yes
    optimise_b=yes
    profile_b=no
    debug_b=yes

    AC_ARG_ENABLE(checking,
    [  --enable-checking       set runtime checks (assert calls).  Default: on],
    [checking_b=$enableval] )

    AC_ARG_ENABLE(debugging,
    [  --enable-debugging      compile with debugging info.  Default: on],
    [debug_b=$enableval])

    AC_ARG_ENABLE(optimising,
    [  --enable-optimising     compile with optimising.  Default: on],
    [optimise_b=$enableval])

    AC_ARG_ENABLE(profiling, 
    [  --enable-profiling      compile with gprof support.  Default: off],
    [profile_b=$enableval])
    

    if test "$checking_b" = no; then
	# ugh
	AC_DEFINE(NDEBUG)
	DEFINES="$DEFINES -DNDEBUG"
    fi

    if test "$optimise_b" = yes; then
	OPTIMIZE="-O2 -finline-functions"
    fi


    if test $profile_b = yes; then
	EXTRA_LIBES="-pg"
	OPTIMIZE="$OPTIMIZE -pg"
    fi

    if test $debug_b = yes; then	
	OPTIMIZE="$OPTIMIZE -g"
    fi


    AC_PROG_CC
    STEPMAKE_OPTIONAL_REQUIRED(CC, cc, $1)
    LD='$(CC)'
    AC_SUBST(LD)

    CFLAGS="$CFLAGS $OPTIMIZE"
    CPPFLAGS=${CPPFLAGS-""}

    AC_MSG_CHECKING([for IEEE-conformance compiler flags])
    save_cflags="$CFLAGS"
    case "$host" in
        alpha*-*-*)
	    dnl should do compile test?
	    AC_MSG_RESULT(-mieee)
	    CFLAGS="-mieee $CFLAGS"
	    ;;
	*)
	    AC_MSG_RESULT([none])
	    ;;
    esac
    AC_SUBST(cross_compiling)
    AC_SUBST(CFLAGS)
    AC_SUBST(CPPFLAGS)
    AC_SUBST(LDFLAGS)
    AC_SUBST(ICFLAGS)
    AC_SUBST(ILDFLAGS)
    AC_SUBST(DEFINES)
    AC_SUBST(EXTRA_LIBES)
])

AC_DEFUN(STEPMAKE_CXX, [
    AC_LANG_CPLUSPLUS
    AC_PROG_CXX
    STEPMAKE_OPTIONAL_REQUIRED(CXX, c++, $1)

    CPPFLAGS="$CPPFLAGS $DEFINES"
    CXXFLAGS="$CXXFLAGS $OPTIMIZE"
    LDFLAGS="$LDFLAGS $EXTRA_LIBES"

    AC_SUBST(CXXFLAGS)
    AC_SUBST(CXX)
    LD='$(CXX)'
    AC_SUBST(LD)
])


AC_DEFUN(STEPMAKE_CXXTEMPLATE, [
    AC_CACHE_CHECK([whether explicit instantiation is needed],
	lily_cv_need_explicit_instantiation,
	AC_TRY_LINK([
    template <class T> struct foo { static int baz; };
    template <class T> int foo<T>::baz = 1;
    ], [ return foo<int>::baz; ],
	    lily_cv_need_explicit_instantiation=no,
	    lily_cv_need_explicit_instantiation=yes))
    if test x"$lily_cv_need_explicit_instantiation"x = x"yes"x; then
	AC_DEFINE(NEED_EXPLICIT_INSTANTIATION)
    fi
])


AC_DEFUN(STEPMAKE_DATADIR, [
    if test "$datadir" = "\${prefix}/share"; then
	    datadir='${prefix}/share'
    fi
    presome=${prefix}
    if test "$prefix" = "NONE"; then
	presome=${ac_default_prefix}
    fi
    
    package_datadir=$datadir/$package
    local_package_datadir=$package_datadir/$FULL_VERSION
    build_package_datadir=$builddir/share/$package
    
    DATADIR=$(echo ${datadir} | sed "s!\\\${prefix}!$presome!")
    PACKAGE_DATADIR=$(echo ${package_datadir} | sed "s!\\\${prefix}!$presome!")
    LOCAL_PACKAGE_DATADIR=$(echo ${local_package_datadir} | sed "s!\\\${prefix}!$presome!")
    BUILD_PACKAGE_DATADIR=$(echo ${build_package_datadir} | sed "s!\\\${prefix}!$presome!")
    
    AC_SUBST(datadir)
    AC_SUBST(package_datadir)
    AC_SUBST(local_package_datadir)
    AC_SUBST(build_package_datadir)
    AC_DEFINE_UNQUOTED(DATADIR, "${DATADIR}")
    AC_DEFINE_UNQUOTED(PACKAGE_DATADIR, "${PACKAGE_DATADIR}")
    AC_DEFINE_UNQUOTED(LOCAL_PACKAGE_DATADIR, "${LOCAL_PACKAGE_DATADIR}")
    AC_DEFINE_UNQUOTED(BUILD_PACKAGE_DATADIR, "${BUILD_PACKAGE_DATADIR}")
])


AC_DEFUN(STEPMAKE_END, [
    AC_SUBST(OPTIONAL)
    AC_SUBST(REQUIRED)
    
    AC_OUTPUT($CONFIGFILE.make:config.make.in)

    
    if test -n "$OPTIONAL"; then
	echo
        echo "WARNING: Please consider installing optional programs: $OPTIONAL"
    fi

    if test -n "$REQUIRED"; then
	echo
        echo "ERROR: Please install required programs: $REQUIRED"
    fi
    
    if test -n "$OPTIONAL$REQUIRED"; then
	echo
	echo "See INSTALL.txt for more information on how to build $PACKAGE_NAME"
	echo "Remove config.cache before rerunning ./configure"
    fi
    
    if test -n "$REQUIRED"; then
	rm -f $srcdir/GNUmakefile
        exit 1
    fi

    # regular in-place build
    # test for srcdir_build = yes ?
    if test "$srcdir_build" = "yes"; then
	rm -f $srcdir/GNUmakefile
	cp $srcdir/GNUmakefile.in $srcdir/GNUmakefile
	chmod 444 $srcdir/GNUmakefile
    else # --srcdir build
        rm -f GNUmakefile
    	cp $srcdir/make/srcdir.make.in GNUmakefile
    	chmod 444 GNUmakefile
    fi
])


AC_DEFUN(STEPMAKE_FLEX, [
    # ugh, automake: we want (and check for) flex
    # AC_PROG_LEX
    # urg: automake 1.3: hope this doesn't break 1.2 ac_cv_pro_lex_root hack...

    # AC_DECL_YYTEXT
    # ugh, ugh
    ac_cv_prog_lex_root=lex.yy
    STEPMAKE_PROGS(FLEX, flex, $1)
])


AC_DEFUN(STEPMAKE_FLEXLEXER, [
    AC_HAVE_HEADERS(FlexLexer.h, true, false)
    if test $? -ne 0; then
	warn='FlexLexer.h (flex package)'
	STEPMAKE_ADD_ENTRY($1, $warn)
    fi
])


AC_DEFUN(STEPMAKE_GCC, [
    if test "$GCC" = "yes"; then
        STEPMAKE_CHECK_VERSION(CC, $1, $2)
    else
	warn="$CC (Please install *GNU* cc)"
	STEPMAKE_ADD_ENTRY($1, $warn)
    fi
])


AC_DEFUN(STEPMAKE_GETTEXT, [
    presome=${prefix}
    if test "$prefix" = "NONE"; then
	    presome=${ac_default_prefix}
    fi
    LOCALEDIR=$(echo ${localedir} | sed "s!\\\${prefix}!$presome!")
    
    AC_SUBST(localedir)
    AC_DEFINE_UNQUOTED(LOCALEDIR, "${LOCALEDIR}")
    AC_CHECK_LIB(intl, gettext)
    AC_CHECK_FUNCS(gettext)
])


AC_DEFUN(STEPMAKE_GUILE, [
    STEPMAKE_PATH_PROG(GUILE, guile, $1)
])


#   STEPMAKE_GUILE_FLAGS --- set flags for compiling and linking with Guile
#
#   This macro runs the guile-config script, installed with Guile,
#   to find out where Guile's header files and libraries are
#   installed.  It sets two variables, marked for substitution, as
#   by AC_SUBST.
#   
#     GUILE_CFLAGS --- flags to pass to a C or C++ compiler to build
#             code that uses Guile header files.  This is almost
#             always just a -I flag.
#   
#     GUILE_LDFLAGS --- flags to pass to the linker to link a
#             program against Guile.  This includes -lguile for
#             the Guile library itself, any libraries that Guile
#             itself requires (like -lqthreads), and so on.  It may
#             also include a -L flag to tell the compiler where to
#             find the libraries.

AC_DEFUN([STEPMAKE_GUILE_FLAGS], [
    exe=`STEPMAKE_GET_EXECUTABLE($guile_config)`
    if test -x $exe; then
	AC_MSG_CHECKING("guile compile flags")
	GUILE_CFLAGS="`$guile_config compile`"
	AC_MSG_RESULT($GUILE_CFLAGS)
	AC_MSG_CHECKING("guile link flags")
	GUILE_LDFLAGS="`$guile_config link`"
	AC_MSG_RESULT($GUILE_LDFLAGS)
    fi
    AC_SUBST(GUILE_CFLAGS)
    AC_SUBST(GUILE_LDFLAGS)
])


AC_DEFUN(STEPMAKE_GUILE_DEVEL, [
    ## First, let's just see if we can find Guile at all.
    AC_MSG_CHECKING("for guile-config")
    for guile_config in guile-config $target-guile-config $build-guile-config; do
	AC_MSG_RESULT("$guile_config")
	if ! $guile_config --version > /dev/null 2>&1 ; then
	    AC_MSG_WARN("cannot execute $guile_config")
	    AC_MSG_CHECKING("if we are cross compiling")
	    GUILE_CONFIG='echo no guile-config'
	else
	    GUILE_CONFIG=$guile_config
	    break
	fi
    done
    STEPMAKE_OPTIONAL_REQUIRED(GUILE_CONFIG, $guile_config, $1)
    if test $? -ne 0; then
        STEPMAKE_ADD_ENTRY($1, 'guile-config (guile-devel, guile-dev or libguile-dev package)')
    fi 

    STEPMAKE_CHECK_SEARCH_RESULT(GUILE_CONFIG)
    # urg.  should test functionality rather than version.
    if test $? -eq 0 -a -n "$2"; then
	STEPMAKE_CHECK_VERSION(GUILE_CONFIG, $1, $2)
    fi

    AC_SUBST(GUILE_CONFIG)
    
    guile_version="$ver"
    changequote(<<, >>)dnl
    GUILE_MAJOR_VERSION=`expr $guile_version : '\([0-9]*\)'`
    GUILE_MINOR_VERSION=`expr $guile_version : '[0-9]*\.\([0-9]*\)'`
    GUILE_PATCH_LEVEL=`expr $guile_version : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
    changequote([, ])dnl
    STEPMAKE_GUILE_FLAGS
    AC_DEFINE_UNQUOTED(GUILE_MAJOR_VERSION, $GUILE_MAJOR_VERSION)
    AC_DEFINE_UNQUOTED(GUILE_MINOR_VERSION, $GUILE_MINOR_VERSION)
    AC_DEFINE_UNQUOTED(GUILE_PATCH_LEVEL, $GUILE_PATCH_LEVEL)
])


AC_DEFUN(STEPMAKE_GXX, [
    if test "$GXX" = "yes"; then
        STEPMAKE_CHECK_VERSION(CXX, $1, $2)
    else
	warn="$CXX (Please install *GNU* c++)"
	STEPMAKE_ADD_ENTRY($1, $warn)
    fi
])


AC_DEFUN(STEPMAKE_INIT, [

    . $srcdir/VERSION
    FULL_VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL
    if test x$MY_PATCH_LEVEL != x; then
	FULL_VERSION=$FULL_VERSION.$MY_PATCH_LEVEL
    fi

    # urg: don't "fix" this: irix doesn't know about [:lower:] and [:upper:]
    changequote(<<, >>)dnl
    PACKAGE=`echo $PACKAGE_NAME | tr '[a-z]' '[A-Z]'`
    package=`echo $PACKAGE_NAME | tr '[A-Z]' '[a-z]'`
    changequote([, ])dnl

    # No versioning on directory names of sub-packages 
    # urg, urg
    stepmake=${datadir}/stepmake
    presome=${prefix}
    if test "$prefix" = "NONE"; then
	    presome=${ac_default_prefix}
    fi
    stepmake=`echo ${stepmake} | sed "s!\\\${prefix}!$presome!"`

    # urg, how is this supposed to work?
    if test "$program_prefix" = "NONE"; then
	  program_prefix=
    fi
    if test "$program_suffix" = "NONE"; then
	  program_suffix=
    fi

    AC_MSG_CHECKING(Package)
    if test "$PACKAGE" = "STEPMAKE"; then
	AC_MSG_RESULT(Stepmake package!)

	AC_MSG_CHECKING(builddir)

	builddir="`pwd`"

	if test "$srcdir" = "."; then
	    srcdir_build=yes
	else
	    srcdir_build=no
	    package_builddir="`dirname $builddir`"
	    package_srcdir="`dirname  $srcdir`"
	fi
	AC_MSG_RESULT($builddir)

	(cd stepmake 2>/dev/null || mkdir stepmake)
	(cd stepmake; rm -f bin; ln -s ../$srcdir/bin .)
	AC_CONFIG_AUX_DIR(bin)
	stepmake=stepmake
    else
        AC_MSG_RESULT($PACKAGE)

	AC_MSG_CHECKING(builddir)
	builddir="`pwd`"
	if test "$srcdir" = "."; then
	    srcdir_build=yes
	else
	    srcdir_build=no
	fi
	AC_MSG_RESULT($builddir)

	AC_MSG_CHECKING(for stepmake)
	# Check for installed stepmake
	if test -d $stepmake; then
	    AC_MSG_RESULT($stepmake)
	else
	    stepmake="`cd $srcdir/stepmake; pwd`"
	    AC_MSG_RESULT($srcdir/stepmake  ($datadir/stepmake not found))
	fi

	AC_CONFIG_AUX_DIR(\
	  $HOME/usr/local/share/stepmake/bin\
	  $HOME/usr/local/lib/stepmake/bin\
	  $HOME/usr/share/stepmake/bin\
	  $HOME/usr/lib/stepmake/bin\
	  /usr/local/share/stepmake/bin\
	  /usr/local/lib/stepmake/bin\
	  /usr/share/stepmake/bin\
	  /usr/lib/stepmake/bin\
	  stepmake/bin\
	  $srcdir/stepmake/bin\
	)
    fi

    AC_SUBST(builddir)
    AC_SUBST(stepmake)
    AC_SUBST(package)
    AC_SUBST(PACKAGE)
    AC_SUBST(PACKAGE_NAME)
    AC_DEFINE_UNQUOTED(PACKAGE, "${PACKAGE_NAME}")
    AC_DEFINE_UNQUOTED(TOPLEVEL_VERSION, "${FULL_VERSION}")

    if test -z "$package_depth"; then
    	package_depth="."
    else
    	package_depth="../$package_depth"
    fi
    export package_depth
    AC_SUBST(package_depth)

    AUTOGENERATE="This file was automatically generated by configure"
    AC_SUBST(AUTOGENERATE)

    CONFIGSUFFIX=
    AC_ARG_ENABLE(config,
    [  --enable-config=CONF    put settings in config-CONF.make and config-CONF.h;
                            do \`make conf=CONF' to get output in ./out-CONF],
    [CONFIGURATION=$enableval])

    ##'

    test -n "$CONFIGURATION" && CONFIGSUFFIX="-$CONFIGURATION"
    CONFIGFILE=config$CONFIGSUFFIX
    AC_SUBST(CONFIGSUFFIX)
     
    AC_CANONICAL_HOST
    STEPMAKE_PROGS(MAKE, gmake make, REQUIRED)
    STEPMAKE_PROGS(FIND, find, REQUIRED)

    STEPMAKE_PROGS(TAR, tar, REQUIRED)

    if test "x`uname`" = "xHP-UX"; then
	AC_PATH_PROG(BASH, bash, /bin/sh)
	STEPMAKE_WARN(avoiding buggy /bin/sh)
	AC_PATH_PROG(SHELL, bash, /bin/ksh)
    else
	AC_PATH_PROG(BASH, bash, /bin/sh)
	SHELL=/bin/sh
	AC_SUBST(SHELL)
    fi

    STEPMAKE_PATH_PROG(PYTHON, python, REQUIRED)

    if expr "$MAKE" : '.*\(echo\)' >/dev/null; then
 	$MAKE -v 2> /dev/null | grep GNU > /dev/null
	if test "$?" = 1; then
	    warn='make (Please install *GNU* make)'
	    # STEPMAKE_WARN($warn)
	    STEPMAKE_ADD_ENTRY(REQUIRED, $warn)
        fi
    fi 

    if test "$OSTYPE" = "cygwin" -o "$OSTYPE" = "cygwin32" -o "$OSTYPE" = "Windows_NT"; then
	LN=cp # hard link does not work under cygnus-nt
	LN_S='cp -r' # symbolic link does not work for native nt
	ZIP="zip -r -9" #
	program_suffix=.exe
 	ROOTSEP=':'
        DIRSEP='/'
 	PATHSEP=':'
	INSTALL="\$(SHELL) \$(stepdir)/../bin/install-dot-exe.sh -c"
    else
	ROOTSEP=':'
	DIRSEP='/'
	PATHSEP=':'
	LN=ln
	LN_S='ln -s'
	ZIP="zip -r -9"
        INSTALL="\$(SHELL) \$(stepdir)/../bin/install-sh -c"
    fi
    AC_SUBST(program_prefix)
    AC_SUBST(program_suffix)
    AC_SUBST(ZIP)
    AC_SUBST(LN)
    AC_SUBST(LN_S)
    AC_SUBST(INSTALL)
    AC_DEFINE_UNQUOTED(DIRSEP, '${DIRSEP}')
    AC_DEFINE_UNQUOTED(PATHSEP, '${PATHSEP}')
    AC_SUBST(DIRSEP)
    AC_SUBST(PATHSEP)
    AC_SUBST(ROOTSEP)
  
    STEPMAKE_DATADIR
])

    
AC_DEFUN(STEPMAKE_KPATHSEA, [
	
    AC_ARG_WITH(kpathsea-include,
	[  --with-kpathsea-include=DIR
	                  location of the kpathsea include dir],[
	    if test "$withval" = "yes" -o "$withval" = "no"; then
		AC_MSG_WARN(Usage: --with-kpathsea-include=includedir)
	    else
		CPPFLAGS="$CPPFLAGS -I${withval}"
	    fi
	    ])
    
    AC_ARG_WITH(kpathsea-lib,
	[  --with-kpathsea-lib=DIR location of the kpathsea lib dir],[
	    if test "$withval" = "yes" -o "$withval" = "no"; then
		AC_MSG_WARN(Usage: --with-kpathsea-lib=libdir)
	    else
		LDFLAGS="$LDFLAGS -L${withval}"
	    fi
	    ])
    
    kpathsea_b=yes
    #FIXME --with-xxx is meant for specifying a PATH too,
    # so this should read: --enable-kpathsea,
    # or --with-kpathsea-include=PATH --with-kpathsea-lib=PATH
    AC_ARG_WITH(kpathsea,
    [  --with-kpathsea         use kpathsea lib.  Default: on],
    [kpathsea_b=$with_kpathsea])

    if test "$kpathsea_b" != "no"; then	
	AC_HAVE_HEADERS(kpathsea/kpathsea.h)
	AC_CHECK_LIB(kpathsea, kpse_find_file)
	AC_CHECK_FUNCS(kpse_find_file,,kpathsea_b=no)
	if test "$kpathsea_b" = "no"; then
	    warn='kpathsea (libkpathsea-dev or kpathsea-devel package)
   Else, please specify the location of your kpathsea using
   --with-kpathea-include and --with-kpathsea-lib options.  You should
   install kpathsea; see INSTALL.txt.  Rerun ./configure
   --without-kpathsea only if kpathsea is not available for your
   platform.'
	    STEPMAKE_ADD_ENTRY(REQUIRED, $warn)
	fi
    fi
    AC_MSG_CHECKING(whether to use kpathsea)
    if test "$kpathsea_b" != no; then
        AC_MSG_RESULT(yes)
	KPATHSEA=1
    else
        AC_MSG_RESULT(no)
	KPATHSEA=0
    fi

    AC_SUBST(KPATHSEA)
    AC_DEFINE_UNQUOTED(KPATHSEA, $KPATHSEA)
])


AC_DEFUN(STEPMAKE_LIB, [
    STEPMAKE_PROGS(AR, ar, $1)
    AC_PROG_RANLIB
    STEPMAKE_OPTIONAL_REQUIRED(RANLIB, ranlib, $1)
])


AC_DEFUN(STEPMAKE_LIBTOOL, [
    # libtool.info ...
    # **Never** try to set library version numbers so that they correspond
    # to the release number of your package.  This is an abuse that only
    # fosters misunderstanding of the purpose of library versions.

    REVISION=$PATCH_LEVEL
    # CURRENT=$MINOR_VERSION
    CURRENT=`expr $MINOR_VERSION + 1`
    # AGE=$(expr $MAJOR_VERSION + 1)
    AGE=$MAJOR_VERSION
    AC_SUBST(CURRENT)
    AC_SUBST(REVISION)
    AC_SUBST(AGE)
])


AC_DEFUN(STEPMAKE_LOCALE, [
    lang=English
    ALL_LINGUAS="en nl"

    # with/enable ??
    AC_ARG_WITH(localedir,
    [  --with-localedir=LOCALE use LOCALE as locale dir.  Default:
                            PREFIX/share/locale ],
    localedir=$with_localedir,
    localedir='${prefix}/share/locale')

    AC_ARG_WITH(lang,
    [  --with-lang=LANG        use LANG as language to emit messages],
    language=$with_lang,
    language=English)

    AC_MSG_CHECKING(language)    
    case "$language" in
      En* | en* | Am* | am* | US* | us*)
	    lang=English;;
      NL | nl | Du* | du* | Ned* | ned*)
	    lang=Dutch;;
      "")
	    lang=English;;
      *)
	    lang=unknown;;
    esac
    AC_MSG_RESULT($lang)

    if test "$lang" = "unknown" ; then
	STEPMAKE_WARN($language not supported; available are: $ALL_LINGUAS)
    fi

])


AC_DEFUN(STEPMAKE_MAKEINFO, [
    STEPMAKE_PROGS(MAKEINFO, makeinfo, $1)
    if test "$MAKEINFO" = "makeinfo"; then
	AC_MSG_CHECKING(whether makeinfo can split html by @node)
	mkdir -p out
	makeinfo --html --output=out/split <<EOF
\input texinfo
\input texinfo @c -*-texinfo-*-
@setfilename split.info
@settitle split.info
@bye
EOF
	if test -d out/split; then
	    SPLITTING_MAKEINFO=yes
	    AC_MSG_RESULT(yes)
	    rm -rf out/split
	else
	    AC_MSG_RESULT(no)
	    STEPMAKE_WARN(your html documentation will be one large file)
	    rm -rf out/split
	fi
    fi
    AC_SUBST(SPLITTING_MAKEINFO)
])



AC_DEFUN(STEPMAKE_MAN, [
    STEPMAKE_PROGS(GROFF, groff ditroff, $1)
    AC_SUBST(GROFF)
    STEPMAKE_PROGS(TROFF, troff, $1)
    AC_SUBST(TROFF)
    STEPMAKE_PROGS(TBL, tbl, $1)
    AC_SUBST(TBL)
])


AC_DEFUN(STEPMAKE_MSGFMT, [
    STEPMAKE_PROGS(MSGFMT, msgfmt, $1)
])


# Check for program ($2), set full path result to ($1).
# If missing, add entry to missing-list ($3, one of 'OPTIONAL', 'REQUIRED')
AC_DEFUN(STEPMAKE_PATH_PROG, [
    AC_CHECK_PROGS($1, $2, no)
    STEPMAKE_OPTIONAL_REQUIRED($1, $2, $3)
    if test $? -eq 0; then
	AC_PATH_PROG($1, $2)
	if test -n "$4"; then
	    STEPMAKE_CHECK_VERSION($1, $3, $4)
	fi
    fi
])


# Check for program in set of names ($2), set result to ($1) .
# If missing, add entry to missing-list ($3, one of 'OPTIONAL', 'REQUIRED')
# If exists, and a minimal version ($4) is required
AC_DEFUN(STEPMAKE_PROGS, [
    AC_CHECK_PROGS($1, $2, no)
    STEPMAKE_OPTIONAL_REQUIRED($1, $2, $3)
    if test $? -eq 0 -a -n "$4"; then
	STEPMAKE_CHECK_VERSION($1, $3, $4)
    fi
])


AC_DEFUN(STEPMAKE_PERL, [
    STEPMAKE_PATH_PROG(PERL, perl, $1)
])


AC_DEFUN(STEPMAKE_PYTHON_DEVEL, [
    AC_HAVE_HEADERS(python2.2/Python.h python2.1/Python.h python2.0/Python.h python2/Python.h python/Python.h python1.5/Python.h Python.h, PYTHON_HEADER=yes)
    if test -z "$PYTHON_HEADER"; then
	warn='python.h (python-devel, python-dev or libpython-dev package)'
	STEPMAKE_ADD_ENTRY($1, $warn)
    fi
])


AC_DEFUN(STEPMAKE_TEXMF_DIRS, [
    AC_ARG_ENABLE(tfm-path,
    [  --enable-tfm-path=PATH  set path of tex directories where tfm files live,
                            esp.: cmr10.tfm.  Default: use kpsewhich],
    [tfm_path=$enableval],
    [tfm_path=auto] )

    # ugh
    STEPMAKE_PROGS(KPSEWHICH, kpsewhich, OPTIONAL)
    AC_MSG_CHECKING(for tfm path)

    TFM_FONTS="cmr msam"

    if test "x$tfm_path" = xauto ; then
	if test "x$KPSEWHICH" != "xno" ; then
	    for i in $TFM_FONTS; do
		dir=`$KPSEWHICH tfm ${i}10.tfm`
		TFM_PATH="$TFM_PATH `dirname $dir`"
	    done
	else
	    STEPMAKE_WARN(Please specify where cmr10.tfm lives:
    ./configure --enable-tfm-path=/usr/local/TeX/lib/tex/fonts)
	fi
    else
         TFM_PATH=$tfm_path
    fi

    TFM_PATH=`echo $TFM_PATH | tr ':' ' '`
    AC_MSG_RESULT($TFM_PATH)
    AC_SUBST(TFM_PATH)
])


AC_DEFUN(STEPMAKE_TEXMF, [
    # urg, never know what names these teTeX guys will think up

    STEPMAKE_PROGS(METAFONT, mf mfont, $1)
    STEPMAKE_PROGS(INIMETAFONT, inimf inimfont, $1)

    AC_MSG_CHECKING(for working metafont mode)
    modelist='ljfour lj4 lj3 lj2 ljet laserjet'
    for MFMODE in $modelist; do
    	$METAFONT "\mode:=$MFMODE; mode_setup; end." > /dev/null 2>&1
	if test -f mfput.tfm; then
	    break;
	fi
    done
    AC_MSG_RESULT($MFMODE)

    rm -f mfput.*

    AC_SUBST(MFMODE)
])


AC_DEFUN(STEPMAKE_WARN, [
    AC_MSG_WARN($1)
    warn_b=yes
])


