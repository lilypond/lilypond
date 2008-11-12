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
    type -p $1 2>/dev/null | tail -n 1 | awk '{print $NF}'
])


# Get version string from executable ($1)
AC_DEFUN(STEPMAKE_GET_VERSION, [
    ## "$1" --version 2>&1 | grep -v '^$' | head -n 1 | awk '{print $NF}'
    ##
    ## ARG.
    ## Workaround for broken Debian gcc version string:
    ##     gcc (GCC) 3.1.1 20020606 (Debian prerelease)
    ##
    ## -V: Workaround for python

    changequote(<<, >>)#dnl

    ## Assume and hunt for dotted version multiplet.
    ## use eval trickery, because we cannot use multi-level $() instead of ``
    ## for compatibility reasons.
    
    ## grab the first version number in  --version output.
    eval _ver=\"\`("$1" --version || "$1" -V) 2>&1 | grep '\(^\| \)[0-9][0-9]*\.[0-9]' \
        | head -n 1 \
	| tr ' ' '\n' | sed 's/\([0-9][0-9]*\.[0-9][0-9.]*\).*/\1/g' | grep '\(^\| \)[0-9][0-9]*\.[0-9]' | head -n 1\`\"

    if test -z "$_ver"; then
        ## If empty, try date [fontforge]
        eval _ver=\"\`("$1" --version || "$1" -V) 2>&1 | grep '[0-9]\{6,8\}' \
	    | head -n 1 \
	    | sed -e 's/^[^.0-9]*//' -e 's/[^.0-9]*$//'\`\"
    fi
    echo "$_ver"
    changequote([, ])#dnl
])

# Calculate simplistic numeric version from version string ($1)
# As yet, we have no need for something more elaborate.
AC_DEFUN(STEPMAKE_NUMERIC_VERSION, [
    echo "$1" | awk -F. '
    {
      if ([$]3) {three = [$]3}
      else {three = 0}
    }
    {printf "%.0f\n", [$]1*1000000 + [$]2*1000 + three}'
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
# If version ($4: optional argument, supply if version cannot be
# parsed using --version or -V ) is smaller than requested ($3), add
# entry to missing-list ($2, one of 'OPTIONAL', 'REQUIRED').
AC_DEFUN(STEPMAKE_CHECK_VERSION, [
    r="`eval echo '$'"$1"`"
    AC_MSG_CHECKING([$r version])
    exe=`STEPMAKE_GET_EXECUTABLE($r)`
    if test -n "$4"; then
        ver="$4"
    else
        ver=`STEPMAKE_GET_VERSION($exe)`
    fi
    num=`STEPMAKE_NUMERIC_VERSION($ver)`
    req=`STEPMAKE_NUMERIC_VERSION($3)`
    AC_MSG_RESULT([$ver])
    if test "$num" -lt "$req"; then
	STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
    vervar="`echo $1 | tr '[a-z]' '[A-Z]'`_VERSION"
    eval `echo $vervar=$num`
##    AC_SUBST(`eval echo $vervar`)
])

# Check version of program ($1)
# If version is greater than or equals unsupported ($3),
# add entry to unsupported list ($2, 'UNSUPPORTED')
AC_DEFUN(STEPMAKE_CHECK_VERSION_UNSUPPORTED, [
    r="`eval echo '$'"$1"`"
    AC_MSG_CHECKING([$r version])
    exe=`STEPMAKE_GET_EXECUTABLE($r)`
    ver=`STEPMAKE_GET_VERSION($exe)`
    num=`STEPMAKE_NUMERIC_VERSION($ver)`
    sup=`STEPMAKE_NUMERIC_VERSION($3)`
    AC_MSG_RESULT([$ver])
    if test "$num" -ge "$sup"; then
	STEPMAKE_ADD_ENTRY($2, ["$r < $3 (installed: $ver)"])
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
    optimise_b=yes
    profile_b=no
    debug_b=yes
    pipe_b=yes

    AC_ARG_ENABLE(debugging,
    [  --enable-debugging      compile with debugging info.  Default: on],
    [debug_b=$enableval])

    AC_ARG_ENABLE(optimising,
    [  --enable-optimising     compile with optimising.  Default: on],
    [optimise_b=$enableval])

    AC_ARG_ENABLE(profiling, 
    [  --enable-profiling      compile with gprof support.  Default: off],
    [profile_b=$enableval])
    
    AC_ARG_ENABLE(pipe, 
    [  --enable-pipe           compile with -pipe.  Default: on],
    [pipe_b=$enableval])

    if test "$optimise_b" = yes; then
	AC_DEFINE(NDEBUG)
	DEFINES="$DEFINES -DNDEBUG"
	OPTIMIZE=" -O2 -finline-functions"
    fi

    if test $profile_b = yes; then
	EXTRA_LIBS="-pg"
	OPTIMIZE="$OPTIMIZE -pg"
    fi

    if test $debug_b = yes; then
	OPTIMIZE="$OPTIMIZE -g"
    fi
 
    AC_PROG_CC
    STEPMAKE_OPTIONAL_REQUIRED(CC, cc, $1)
    LD='$(CC)'
    AC_SUBST(LD)

    # If -pipe requested, test if it works and add to CFLAGS.
    if test "$pipe_b" = yes; then
	save_cflags="$CFLAGS"
	CFLAGS=" -pipe $CFLAGS";
	AC_CACHE_CHECK([whether compiler understands -pipe],
	    [stepmake_cflags_pipe],
	    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[/* -pipe test */]])],
		[stepmake_cflags_pipe=yes],
		[stepmake_cflags_pipe=no]))
	CFLAGS=$save_cflags
	if test $stepmake_cflags_pipe = yes; then
	    OPTIMIZE="$OPTIMIZE -pipe"
	fi
    fi

    CFLAGS="$CFLAGS $OPTIMIZE"
    CPPFLAGS=${CPPFLAGS-""}

    AC_MSG_CHECKING([for IEEE-conformance compiler flags])
    save_cflags="$CFLAGS"
    case "$host" in
        alpha*-*-*)
	    dnl should do compile test?
	    AC_MSG_RESULT(-mieee)
	    CFLAGS=" -mieee $CFLAGS"
	    ;;
	*)
	    AC_MSG_RESULT([none])
	    ;;
    esac

    AC_SUBST(cross_compiling)
    AC_SUBST(CFLAGS)
    AC_SUBST(CPPFLAGS)
    AC_SUBST(LDFLAGS)
    AC_SUBST(DEFINES)
    AC_SUBST(EXTRA_LIBS)
])

AC_DEFUN(STEPMAKE_CXX, [
    AC_LANG([C++])
    AC_PROG_CXX
    STEPMAKE_OPTIONAL_REQUIRED(CXX, c++, $1)

    CXXFLAGS="$CXXFLAGS $OPTIMIZE"
    LD='$(CXX)'

    AC_SUBST(CXX)
    AC_SUBST(CXXFLAGS)
    AC_SUBST(LD)
])


AC_DEFUN(STEPMAKE_CXXTEMPLATE, [
    AC_CACHE_CHECK([whether explicit instantiation is needed],
	lily_cv_need_explicit_instantiation,
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
    template <class T> struct foo { static int baz; };
    template <class T> int foo<T>::baz = 1;
    ]], [[ return foo<int>::baz; ]])],[lily_cv_need_explicit_instantiation=no],[lily_cv_need_explicit_instantiation=yes]))
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
    
    build_package_datadir=$ugh_ugh_autoconf250_builddir/out$CONFIGSUFFIX/share/$package
    
    DATADIR=`echo ${datadir} | sed "s!\\\${datarootdir}!${prefix}/share!"`
    DATADIR=`echo ${DATADIR} | sed "s!\\\${prefix}!$presome!"`
    BUILD_PACKAGE_DATADIR=`echo ${build_package_datadir} | sed "s!\\\${prefix}!$presome!"`
    
    AC_SUBST(datadir)
    AC_SUBST(datarootdir)
    AC_SUBST(build_package_datadir)
    AC_DEFINE_UNQUOTED(DATADIR, ["${DATADIR}"])
    AC_DEFINE_UNQUOTED(BUILD_PACKAGE_DATADIR, ["${BUILD_PACKAGE_DATADIR}"])
])

## ugh: cut & paste programming from datadir. 
AC_DEFUN(STEPMAKE_LIBDIR, [

    if test "$libdir" = "\${exec_prefix}/lib"; then
 	libdir='${exec_prefix}/lib'
    fi
    presome=$exec_prefix
    build_package_libdir=$ugh_ugh_autoconf250_builddir/out$CONFIGSUFFIX/lib/$package
    
    LIBDIR=`echo ${libdir} | sed "s!\\\${exec_prefix}!$presome!"`
    BUILD_PACKAGE_LIBDIR=`echo ${build_package_libdir} | sed "s!\\\${exec_prefix}!$presome!"`
    
    AC_SUBST(libdir)
    AC_SUBST(build_package_libdir)
    AC_DEFINE_UNQUOTED(LIBDIR, ["${LIBDIR}"])
    AC_DEFINE_UNQUOTED(BUILD_PACKAGE_LIBDIR, ["${BUILD_PACKAGE_LIBDIR}"])
])


AC_DEFUN(STEPMAKE_END, [
    AC_SUBST(OPTIONAL)
    AC_SUBST(REQUIRED)
    
    AC_CONFIG_FILES([$CONFIGFILE.make:config.make.in])
    AC_OUTPUT
    
    if test -n "$OPTIONAL"; then
	echo
        echo "WARNING: Please consider installing optional programs: $OPTIONAL"
    fi

    if test -n "$REQUIRED"; then
	echo
        echo "ERROR: Please install required programs: $REQUIRED"
    fi
    
    if test -n "$UNSUPPORTED"; then
	echo
        echo "ERROR: Please use older version of programs: $UNSUPPORTED"
    fi
    
    if test -n "$OPTIONAL$REQUIRED$UNSUPPORTED"; then
	echo
	echo "See INSTALL.txt for more information on how to build $PACKAGE_NAME"
	if test -f config.cache ; then
	    echo "Remove config.cache before rerunning ./configure"
	fi 
    fi
    
    if test -n "$REQUIRED$UNSUPPORTED"; then
	rm -f $srcdir/GNUmakefile
        exit 1
    fi

    # regular in-place build
    # test for srcdir_build = yes ?
    if test "$srcdir_build" = "yes"; then
	rm -f $srcdir/GNUmakefile
	cp $srcdir/GNUmakefile.in $srcdir/GNUmakefile
	chmod 444 $srcdir/GNUmakefile
    else
	if test -f $srcdir/GNUmakefile; then
	    cat <<EOF
Source directory already configured.  Please clean the source directory

     make -C $srcdir distclean

and rerun configure.
EOF
	    exit 2
	fi

	for d in 2 3 4 ; do
	    for mf in `cd $srcdir ; find -maxdepth $d -mindepth $d -name GNUmakefile`; do
		mkdir -p $(dirname $mf)
	        cat <<EOF | $PYTHON -  > $mf
print 'depth=' + ('../' * ( $d-1 ) )
print 'include \$(depth)/config\$(if \$(conf),-\$(conf),).make'
print 'include \$(configure-srcdir)/$mf'
EOF
	    done
	    for mf in `cd $srcdir ; find -maxdepth $d -mindepth $d -name '*.make' | grep -v config.make `; do
		mkdir -p $(dirname $mf)
	        cat <<EOF | $PYTHON -  > $mf
print 'include \$(depth)/config\$(if \$(conf),-\$(conf),).make'
print 'include \$(configure-srcdir)/$mf'
EOF
	    done
	done

	
	cat <<EOF > GNUmakefile
depth = ./
include config\$(if \$(conf),-\$(conf),).make
include \$(configure-srcdir)/GNUmakefile.in
EOF
	AC_SUBST(VPATH)
    fi
])


AC_DEFUN(STEPMAKE_FLEX, [
    # ugh, automake: we want (and check for) flex
    # AC_PROG_LEX
    # urg: automake 1.3: hope this doesn't break 1.2 ac_cv_pro_lex_root hack...

    # AC_PROG_LEX()
    # ugh, ugh
    ac_cv_prog_lex_root=lex.yy
    STEPMAKE_PROGS(FLEX, flex, $1)
])


AC_DEFUN(STEPMAKE_FLEXLEXER, [
    AC_CHECK_HEADERS([FlexLexer.h],[true],[false])
    if test $? -ne 0; then
	warn='FlexLexer.h (flex package)'
	STEPMAKE_ADD_ENTRY($1, $warn)
    fi
    # check for yyFlexLexer.yy_current_buffer,
    # in 2.5.4 <= flex < 2.5.29
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([for yyFlexLexer.yy_current_buffer],
	[stepmake_flexlexer_yy_current_buffer],
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
using namespace std;
#include <FlexLexer.h>
class yy_flex_lexer: public yyFlexLexer
{
  public:
    yy_flex_lexer ()
    {
      yy_current_buffer = 0;
    }
};
]])],
	    [stepmake_flexlexer_yy_current_buffer=yes],
	    [stepmake_flexlexer_yy_current_buffer=no]))
    if test $stepmake_flexlexer_yy_current_buffer = yes; then
	AC_DEFINE(HAVE_FLEXLEXER_YY_CURRENT_BUFFER, 1, [Define to 1 if yyFlexLexer has yy_current_buffer.])
    fi
    AC_LANG_POP(C++)
])
  


AC_DEFUN(STEPMAKE_FLEXLEXER_LOCATION, [
	AC_MSG_CHECKING([FlexLexer.h location])

	# ugh.
	cat <<EOF > conftest.cc
using namespace std;
#include <FlexLexer.h>
EOF
	FLEXLEXER_FILE=`eval $ac_cpp conftest.cc | \
	  sed 's!# 1 "\(.*FlexLexer.h\)"!@FLEXLEXER@\1@@!g' | grep '@@' | \
	  sed 's!.*@FLEXLEXER@\(.*\)@@.*$!\1!g' ` 1> /dev/null 2> /dev/null
	rm conftest.cc
	AC_SUBST(FLEXLEXER_FILE)
        AC_MSG_RESULT($FLEXLEXER_FILE)
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
    LOCALEDIR=`echo ${localedir} | sed "s!\\\${prefix}!$presome!"`
    
    AC_SUBST(localedir)
    AC_DEFINE_UNQUOTED(LOCALEDIR, ["${LOCALEDIR}"])
    # ouch.  autoconf <= 2.57's gettext check fails for
    # g++ >= 3.3 (with -std=gnu++98, the default).
    # While the check is OK for g++ -std=c++98,
    # LilyPond needs GNU g++, so who is to blame here?
    # Use a workaround until this is resolved:
    # for g++ >= 3.3, select C language.
    GCC_UNSUPPORTED=
    STEPMAKE_CHECK_VERSION_UNSUPPORTED(CXX, GCC_UNSUPPORTED, 3.3)
    if test -n "$GCC_UNSUPPORTED"; then
	AC_MSG_WARN([autoconf <= 2.59 with g++ >= 3.3 gettext test broken.])
	AC_MSG_WARN([Trying gcc, cross fingers.])
	AC_LANG_PUSH(C)
    fi
    AC_CHECK_LIB(intl, gettext)
    AC_CHECK_FUNCS(gettext)
    if test -n "$GCC_UNSUPPORTED"; then
	AC_LANG_POP(C)
    fi
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
	AC_MSG_CHECKING([guile compile flags])
	GUILE_CFLAGS="`$guile_config compile`"
	AC_MSG_RESULT($GUILE_CFLAGS)
	AC_MSG_CHECKING([guile link flags])
	GUILE_LDFLAGS="`$guile_config link`"
	AC_MSG_RESULT($GUILE_LDFLAGS)
    fi
    AC_SUBST(GUILE_CFLAGS)
    AC_SUBST(GUILE_LDFLAGS)
])


AC_DEFUN(STEPMAKE_GUILE_DEVEL, [
    ## First, let's just see if we can find Guile at all.
    test -n "$target_alias" && target_guile_config=$target_alias-guile-config
    test -n "$host_alias" && host_guile_config=$host_alias-guile-config
    AC_MSG_CHECKING([for guile-config])
    for guile_config in $GUILE_CONFIG $target_guile_config $host_guile_config $build_guile_config guile-config; do
	AC_MSG_RESULT([$guile_config])
	if ! $guile_config --version > /dev/null 2>&1 ; then
	    AC_MSG_WARN([cannot execute $guile_config])
	    AC_MSG_CHECKING([if we are cross compiling])
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
    changequote(<<, >>)#dnl
    GUILE_MAJOR_VERSION=`expr $guile_version : '\([0-9]*\)'`
    GUILE_MINOR_VERSION=`expr $guile_version : '[0-9]*\.\([0-9]*\)'`
    GUILE_PATCH_LEVEL=`expr $guile_version : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
    changequote([, ])#dnl
    STEPMAKE_GUILE_FLAGS
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    CPPFLAGS="$GUILE_CFLAGS $CPPFLAGS"
    LIBS="$GUILE_LDFLAGS $LIBS"
    AC_CHECK_HEADERS([libguile.h])
    AC_CHECK_LIB(guile, scm_boot_guile)
    AC_CHECK_FUNCS(scm_boot_guile,,libguile_b=no)
    if test "$libguile_b" = "no"; then
	    warn='libguile (libguile-dev, guile-devel or guile-dev
   package).'
	    STEPMAKE_ADD_ENTRY(REQUIRED, $warn)
    fi
    CPPFLAGS="$save_CPPFLAGS"
    LIBS="$save_LIBS"
    AC_DEFINE_UNQUOTED(GUILE_MAJOR_VERSION, $GUILE_MAJOR_VERSION)
    AC_DEFINE_UNQUOTED(GUILE_MINOR_VERSION, $GUILE_MINOR_VERSION)
    AC_DEFINE_UNQUOTED(GUILE_PATCH_LEVEL, $GUILE_PATCH_LEVEL)
])


AC_DEFUN(STEPMAKE_DLOPEN, [
    AC_CHECK_LIB(dl, dlopen)
    AC_CHECK_FUNCS(dlopen)
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

    AC_PREREQ(2.50)
    . $srcdir/VERSION
    FULL_VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL
    MICRO_VERSION=$PATCH_LEVEL
    TOPLEVEL_VERSION=$FULL_VERSION
    if test x$MY_PATCH_LEVEL != x; then
	FULL_VERSION=$FULL_VERSION.$MY_PATCH_LEVEL
    fi
    VERSION=$FULL_VERSION
    export MAJOR_VERSION MINOR_VERSION PATCH_LEVEL
    # urg: don't "fix" this: irix doesn't know about [:lower:] and [:upper:]
    changequote(<<, >>)#dnl
    PACKAGE=`echo $PACKAGE_NAME | tr '[a-z]' '[A-Z]'`
    package=`echo $PACKAGE_NAME | tr '[A-Z]' '[a-z]'`
    changequote([, ])#dnl

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

	ugh_ugh_autoconf250_builddir="`pwd`"
	
	if test "$srcdir" = "."; then
	    srcdir_build=yes
	else
	    srcdir_build=no
	    package_builddir="`dirname $ugh_ugh_autoconf250_builddir`"
	    package_srcdir="`dirname  $srcdir`"
	fi
	AC_MSG_RESULT($ugh_ugh_autoconf250_builddir)

	(cd stepmake 2>/dev/null || mkdir stepmake)
	(cd stepmake; rm -f bin; ln -s ../$srcdir/bin .)
	stepmake=stepmake
    else
        AC_MSG_RESULT($PACKAGE)

	AC_MSG_CHECKING(builddir)
	ugh_ugh_autoconf250_builddir="`pwd`"

	here_dir=$(cd . && pwd)
	full_src_dir=$(cd $srcdir && pwd)

	if test "$full_src_dir" = "$here_dir"; then
	    srcdir_build=yes
	else
	    srcdir_build=no
	fi
	AC_MSG_RESULT($ugh_ugh_autoconf250_builddir)

	AC_MSG_CHECKING(for stepmake)
	# Check for installed stepmake
	if test -d $stepmake; then
	    AC_MSG_RESULT($stepmake)
	else
	    stepmake="`cd $srcdir/stepmake; pwd`"
	    AC_MSG_RESULT([$srcdir/stepmake  ($datadir/stepmake not found)])
	fi
    fi

    AC_SUBST(ugh_ugh_autoconf250_builddir)

    # Use absolute directory for non-srcdir builds, so that build
    # dir can be moved.
    if test "$srcdir_build" = "no" ;  then 
	srcdir="`cd $srcdir; pwd`"
    fi
    
    AC_SUBST(srcdir)
    AC_SUBST(stepmake)
    AC_SUBST(package)
    AC_SUBST(PACKAGE)
    AC_SUBST(PACKAGE_NAME)
    AC_SUBST(VERSION)
    AC_SUBST(MAJOR_VERSION)
    AC_SUBST(MINOR_VERSION)
    AC_SUBST(MICRO_VERSION)

    # stepmake nonstandard names
    AC_SUBST(PATCH_LEVEL)
    AC_SUBST(TOPLEVEL_VERSION)
    
    # We don't need the upper case variant,
    # so stick to macros are uppercase convention.
    # AC_DEFINE_UNQUOTED(package, ["${package}"])
    # AC_DEFINE_UNQUOTED(PACKAGE, ["${PACKAGE}"])
    AC_DEFINE_UNQUOTED(PACKAGE, ["${package}"])
    AC_DEFINE_UNQUOTED(PACKAGE_NAME, ["${PACKAGE_NAME}"])
    AC_DEFINE_UNQUOTED(TOPLEVEL_VERSION, ["${FULL_VERSION}"])

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
                            do `make conf=CONF' to get output in ./out-CONF],
    [CONFIGURATION=$enableval])

    ##'`#

    test -n "$CONFIGURATION" && CONFIGSUFFIX="-$CONFIGURATION"
    CONFIGFILE=config$CONFIGSUFFIX
    AC_SUBST(CONFIGSUFFIX)
     
    AC_CANONICAL_HOST
    STEPMAKE_PROGS(MAKE, gmake make, REQUIRED)
    STEPMAKE_PROGS(FIND, find, REQUIRED)

    STEPMAKE_PROGS(TAR, tar, REQUIRED)

    if test "$(echo 2)" != "2" ||
	test "x`uname`" = "xHP-UX"; then
	AC_PATH_PROG(KSH, ksh, /bin/ksh)
	AC_PATH_PROG(BASH, bash, $KSH)
	STEPMAKE_WARN(avoiding buggy /bin/sh)
	AC_PATH_PROG(SHELL, bash, $KSH)
    else
	SHELL=/bin/sh
	AC_PATH_PROG(BASH, bash, $SHELL)
    fi
    AC_SUBST(SHELL)

    STEPMAKE_PYTHON(REQUIRED, 1.5)

    if expr "$MAKE" : '.*\(echo\)' >/dev/null; then
 	$MAKE -v 2> /dev/null | grep GNU > /dev/null
	if test "$?" = 1; then
	    warn='make (Please install *GNU* make)'
	    # STEPMAKE_WARN($warn)
	    STEPMAKE_ADD_ENTRY(REQUIRED, $warn)
        fi
    fi

    ROOTSEP=':'
    DIRSEP='/'
    PATHSEP=':'
    LN=ln
    LN_S='ln -s'
    ZIP="zip -r -9"

    AC_SUBST(program_prefix)
    AC_SUBST(program_suffix)
    AC_SUBST(ZIP)
    AC_SUBST(LN)
    AC_SUBST(LN_S)
    AC_DEFINE_UNQUOTED(DIRSEP, ['${DIRSEP}'])
    AC_DEFINE_UNQUOTED(PATHSEP, ['${PATHSEP}'])
    AC_SUBST(DIRSEP)
    AC_SUBST(PATHSEP)
    AC_SUBST(ROOTSEP)
  
    STEPMAKE_DATADIR
    STEPMAKE_LIBDIR
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
    AC_ARG_ENABLE(kpathsea,
    [  --enable-kpathsea         use kpathsea lib.  Default: on],
    [kpathsea_b=$enableval])

    save_LIBS="$LIBS"
    if test "$kpathsea_b" != "no"; then	
	AC_CHECK_HEADERS([kpathsea/kpathsea.h],,kpathsea_b=no)
	AC_CHECK_LIB(kpathsea, kpse_find_file)
	AC_CHECK_FUNCS(kpse_find_file,,kpathsea_b=no)
	if test "$kpathsea_b" = "no"; then
	    STEPMAKE_ADD_ENTRY(OPTIONAL, $warn)
	fi
    fi

    save_CFLAGS="$CFLAGS"
    CFLAGS=`echo "-shared $CFLAGS" | sed -e 's/ -g//'`
    AC_MSG_CHECKING([for shared libkpathsea])
    AC_TRY_LINK([#include <kpathsea/kpathsea.h>],
                 [kpse_var_expand ("\$TEXMF");],
                 [have_libkpathsea_so=maybe;
		  shared_size=`wc -c conftest$ac_exeext`;
		  shared_size=`echo $shared_size | sed -e 's/ .*//g'`],
                 [have_libkpathsea_so=no])

    if test "$have_libkpathsea_so" = "maybe"; then
	if test "$shared_size" -lt 40000 ; then
	  have_libkpathsea_so=yes
	else
	  have_libkpathsea_so=no
	fi
    fi
    
    AC_MSG_RESULT($have_libkpathsea_so)
    if test "$have_libkpathsea_so" = "yes"; then
	AC_DEFINE(HAVE_LIBKPATHSEA_SO)
    fi
    CFLAGS="$save_CFLAGS"

    KPATHSEA_LIBS="$LIBS"
    LIBS="$save_LIBS"
    AC_MSG_CHECKING(whether to use kpathsea)
    if test "$kpathsea_b" != no; then
        AC_MSG_RESULT(yes)
	KPATHSEA=1
    else
        AC_MSG_RESULT(no)
	KPATHSEA=0
    fi

    AC_SUBST(KPATHSEA)
    AC_SUBST(KPATHSEA_LIBS)
    AC_SUBST(HAVE_LIBKPATHSEA_SO, $have_libkpathsea_so)
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
    # AGE=`expr $MAJOR_VERSION + 1`
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
    [  --with-localedir=DIR    location of locales.  Default: PREFIX/share/locale ],
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


AC_DEFUN(STEPMAKE_PYTHON, [
    unset pv
    AC_MSG_CHECKING([for python])
    for python in $PYTHON python python2 python2.4 python2.3 python2.2 python2.1 python2.0; do
	AC_MSG_RESULT([$python])
	if ! $python -V > /dev/null 2>&1 ; then
	    #AC_MSG_WARN([cannot execute $python])
	    PYTHON='echo no python'
	else
	    unset pv
	    STEPMAKE_CHECK_VERSION(python, pv, $2)
	    if test -z "$pv"; then
		PYTHON=$python
		break
	    fi
	fi
    done
    if test -n "$pv"; then
	STEPMAKE_ADD_ENTRY($1, $pv)
    fi
    # clear cached value since arg 2 might point us to a new binary
    unset ac_cv_path_PYTHON

    AC_PATH_PROG(PYTHON, $PYTHON)
    AC_SUBST(PYTHON)
])

AC_DEFUN(STEPMAKE_PYTHON_DEVEL, [
    AC_ARG_WITH(python-include,
	[  --with-python-include=DIR
	                  location of the python include dir],[
	    if test "$withval" = "yes" -o "$withval" = "no"; then
		AC_MSG_WARN(Usage: --with-python-include=includedir)
	    else
		PYTHON_CFLAGS="-I${withval}"
	    fi
	    ])
    
    AC_ARG_WITH(python-lib,
	[  --with-python-lib=NAME  name of the python lib],[
	    if test "$withval" = "yes" -o "$withval" = "no"; then
		AC_MSG_WARN(Usage: --with-python-lib=name)
	    else
		LDFLAGS="$LDFLAGS -l${withval}"
	    fi
	    ])
    
    AC_CHECK_PROGS(PYTHON_CONFIG, python-config, no)

    if test -z "$PYTHON_CFLAGS" -a "$PYTHON_CONFIG" != "no"; then
        # Clean out junk: http://bugs.python.org/issue3290
	# Python headers may need some -f* flags, leave them in.
	PYTHON_CFLAGS=`$PYTHON_CONFIG --cflags | sed -e 's/ -\(W\|D\|O\|m\)\(\w\|-\|=\)\+//g'`
	PYTHON_LDFLAGS=`$PYTHON_CONFIG --ldflags`
    fi
    
    if test -z "$PYTHON_CFLAGS" -a "$cross_compiling" = "no"; then
	changequote(<<, >>)#dnl
	# alternatively, for python >= 2.0
	# 'import sys, distutils.sysconfig; sys.stdout.write (distutils.sysconfig.get_python_inc ())'
	PYTHON_INCLUDE=`$PYTHON -c 'import sys; sys.stdout.write ("%s/include/python%s" % (sys.prefix, sys.version[:3]))'`
	PYTHON_CFLAGS="-I$PYTHON_INCLUDE"
	changequote([, ])#dnl
    fi
    
    if test -z "$PYTHON_HEADER"; then
	CPPFLAGS="$PYTHON_CFLAGS $CPPFLAGS"
	AC_CHECK_HEADERS([Python.h],[PYTHON_HEADER=yes])
    fi
    
    if test -z "$PYTHON_HEADER"; then
	warn="Python.h (python-devel, python-dev or libpython-dev package)"
	STEPMAKE_ADD_ENTRY($1, $warn)
    fi
])



AC_DEFUN(STEPMAKE_STL_DATA_METHOD, [
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([for stl.data () method],
	[stepmake_stl_data_method],
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <vector>
using namespace std;
vector <int> v;
void *p = v.data ();
]])],
	    [stepmake_stl_data_method=yes],
	    [stepmake_stl_data_method=no]))
    if test $stepmake_stl_data_method = yes; then
	AC_DEFINE(HAVE_STL_DATA_METHOD, 1, [define if stl classes have data () method])
    fi
    AC_LANG_POP(C++)
])


AC_DEFUN(STEPMAKE_TEXMF_DIRS, [
    # ugh
    STEPMAKE_PROGS(KPSEWHICH, kpsewhich, OPTIONAL)
])

AC_DEFUN(STEPMAKE_TEXMF, [
    STEPMAKE_PROGS(METAFONT, mf-nowin mf mfw mfont, $1)
    STEPMAKE_PROGS(METAPOST, mpost, $1)
    # STEPMAKE_PROGS(INIMETAFONT, inimf inimfont "$METAFONT -ini", $1)

    AC_MSG_CHECKING(for working metafont mode)
    modelist='ljfour lj4 lj3 lj2 ljet laserjet'
    for MFMODE in $modelist; do
    	$METAFONT -progname=mf "\mode:=$MFMODE; mode_setup; end." > /dev/null 2>&1
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


dnl PKG_CHECK_MODULES(GSTUFF, gtk+-2.0 >= 1.3 glib = 1.3.4, action-if, action-not)
dnl defines GSTUFF_LIBS, GSTUFF_CFLAGS, see pkg-config man page
dnl also defines GSTUFF_PKG_ERRORS on error
AC_DEFUN(PKG_CHECK_MODULES, [
  succeeded=no

  if test -z "$PKG_CONFIG"; then
    AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  fi

  if test "$PKG_CONFIG" = "no" ; then
     echo "*** The pkg-config script could not be found. Make sure it is"
     echo "*** in your path, or set the PKG_CONFIG environment variable"
     echo "*** to the full path to pkg-config."
     echo "*** Or see http://www.freedesktop.org/software/pkgconfig to get pkg-config."
  else
     PKG_CONFIG_MIN_VERSION=0.9.0
     if $PKG_CONFIG --atleast-pkgconfig-version $PKG_CONFIG_MIN_VERSION; then
        AC_MSG_CHECKING(for $2)

        if $PKG_CONFIG --exists "$2" ; then
            AC_MSG_RESULT(yes)
            succeeded=yes

            AC_MSG_CHECKING($1_CFLAGS)
            $1_CFLAGS=`$PKG_CONFIG --cflags "$2"`
            AC_MSG_RESULT($$1_CFLAGS)

            AC_MSG_CHECKING($1_LIBS)
            $1_LIBS=`$PKG_CONFIG --libs "$2"`
            AC_MSG_RESULT($$1_LIBS)
        else
            $1_CFLAGS=""
            $1_LIBS=""
            ## If we have a custom action on failure, don't print errors, but 
            ## do set a variable so people can do so.
            $1_PKG_ERRORS=`$PKG_CONFIG --errors-to-stdout --print-errors "$2"`
            ifelse([$4], ,echo $$1_PKG_ERRORS,)
        fi

        AC_SUBST($1_CFLAGS)
        AC_SUBST($1_LIBS)
     fi
  fi

  if test $succeeded = yes; then
     ifelse([$3], , :, [$3])
  else
     ifelse([$4], , AC_MSG_ERROR([Library requirements ($2) not met; consider adjusting the PKG_CONFIG_PATH environment variable if your libraries are in a nonstandard prefix so pkg-config can find them.]), [$4])
  fi
])

AC_DEFUN(STEPMAKE_FREETYPE2, [
    PKG_CHECK_MODULES(FREETYPE2, $1 >= $3, have_freetype2=yes, true)
    if test "$have_freetype2" = yes; then
	AC_DEFINE(HAVE_FREETYPE2)
        save_CPPFLAGS="$CPPFLAGS"
        save_LIBS="$LIBS"
	CPPFLAGS="$FREETYPE2_CFLAGS $CPPFLAGS"
	LIBS="$FREETYPE2_LIBS $LIBS"
	AC_SUBST(FREETYPE2_CFLAGS)
	AC_SUBST(FREETYPE2_LIBS)
	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
    else
	# UGR
     	#r="lib$1-dev or $1-devel"
     	r="libfreetype6-dev or freetype?-devel"
     	ver="`pkg-config --modversion $1`"
     	STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_GTK2, [
    PKG_CHECK_MODULES(GTK2, $1 >= $3, have_gtk2=yes, true)
    if test "$have_gtk2" = yes ; then
	AC_DEFINE(HAVE_GTK2)
	# Do not pollute user-CPPFLAGS with configure-CPPFLAGS
        save_CPPFLAGS="$CPPFLAGS"
        save_LIBS="$LIBS"
	CPPFLAGS="$GTK2_CFLAGS $CPPFLAGS"
	LIBS="$GTK2_LIBS $LIBS"
	AC_SUBST(GTK2_CFLAGS)
	AC_SUBST(GTK2_LIBS)
	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
    else
	# UGR
     	# r="lib$1-dev or $1-devel"
     	r="libgtk2.0-dev or gtk2-devel"
     	ver="`pkg-config --modversion $1`"
     	STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_PANGO, [
    PKG_CHECK_MODULES(PANGO, $1 >= $3, have_pango16=yes, true)
    if test "$have_pango16" = yes ; then
	AC_DEFINE(HAVE_PANGO16)
	# Do not pollute user-CPPFLAGS with configure-CPPFLAGS
        save_CPPFLAGS="$CPPFLAGS"
        save_LIBS="$LIBS"
	CPPFLAGS="$PANGO_CFLAGS $CPPFLAGS"
	LIBS="$PANGO_LIBS $LIBS"
	AC_CHECK_HEADERS([pango/pangofc-fontmap.h])
	AC_CHECK_FUNCS([pango_fc_font_map_add_decoder_find_func])
	AC_SUBST(PANGO_CFLAGS)
	AC_SUBST(PANGO_LIBS)
	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
    else
	# UGR
     	#r="lib$1-dev or $1-devel"
     	r="libpango1.0-dev or pango1.0-devel"
     	ver="`pkg-config --modversion $1`"
     	STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_PANGO_FT2, [
    PKG_CHECK_MODULES(PANGO_FT2, $1 >= $3, have_pangoft2=yes, true)
    if test "$have_pangoft2" = yes ; then
	AC_DEFINE(HAVE_PANGO16)
	AC_DEFINE(HAVE_PANGO_FT2)
	# Do not pollute user-CPPFLAGS with configure-CPPFLAGS
        save_CPPFLAGS="$CPPFLAGS"
        save_LIBS="$LIBS"
	CPPFLAGS="$CPPFLAGS $PANGO_FT2_CFLAGS"
	LIBS="$PANGO_FT2_LIBS $LIBS"
	AC_CHECK_HEADERS([pango/pangoft2.h])
	AC_CHECK_FUNCS([pango_ft2_font_map_create_context])
	AC_SUBST(PANGO_FT2_CFLAGS)
	AC_SUBST(PANGO_FT2_LIBS)
	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
    else
	# UGR
     	#r="lib$1-dev or $1-devel"e
     	r="libpango1.0-dev or pango?-devel"
     	ver="`pkg-config --modversion $1`"
     	STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_FONTCONFIG, [
    PKG_CHECK_MODULES(FONTCONFIG, $1 >= $3, have_fontconfig=yes, true)
    if test "$have_fontconfig" = yes ; then
	AC_DEFINE(HAVE_FONTCONFIG)
	# Do not pollute user-CPPFLAGS with configure-CPPFLAGS
        save_CPPFLAGS="$CPPFLAGS"
        save_LIBS="$LIBS"
	CPPFLAGS="$FONTCONFIG_CFLAGS $CPPFLAGS"
	LIBS="$FONTCONFIG_LIBS $LIBS"
	AC_SUBST(FONTCONFIG_CFLAGS)
	AC_SUBST(FONTCONFIG_LIBS)
	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
    else
     	r="lib$1-dev or $1-devel"
     	ver="`pkg-config --modversion $1`"
     	STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_WINDOWS, [
    AC_CYGWIN
    AC_MINGW32

    if test "$CYGWIN" = "yes"; then
	LN_S='cp -r' # Cygwin symbolic links do not work for native apps.
	program_suffix=.exe
	INSTALL="\$(SHELL) \$(stepdir)/../bin/install-dot-exe.sh -c"
    elif test "$MINGW32" = "yes"; then
	LN='cp -r'
	LN_S='cp -r'
	program_suffix=.exe
	INSTALL="\$(SHELL) \$(stepdir)/../bin/install-dot-exe.sh -c"
	PATHSEP=';'
    fi

    AC_SUBST(LN)
    AC_SUBST(LN_S)
    AC_DEFINE_UNQUOTED(DIRSEP, ['${DIRSEP}'])
    AC_DEFINE_UNQUOTED(PATHSEP, ['${PATHSEP}'])
    AC_SUBST(DIRSEP)
    AC_SUBST(PATHSEP)
    AC_SUBST(program_suffix)

    AC_MSG_CHECKING([for some flavor of Windows])
    if test "$CYGWIN$MINGW32" = "nono"; then
        PLATFORM_WINDOWS=no
    else
        PLATFORM_WINDOWS=yes
    fi
    AC_MSG_RESULT([$PLATFORM_WINDOWS])
    AC_SUBST(PLATFORM_WINDOWS)
    STEPMAKE_PROGS(WINDRES, $target-windres windres, x)
    AC_SUBST(WINDRES)
])
