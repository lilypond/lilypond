dnl aclocal.m4   -*-shell-script-*-
dnl StepMake subroutines for configure.ac

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
#                           Jan Nieuwenhuizen <janneke@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


### mostly internal macros

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

    ## grab the first version number in --version output.
    eval _ver=\"\`("$1" --version || "$1" -V) 2>&1 |
        grep -E '(^| )[0-9][0-9]*\.[0-9]' |
        head -n 1 |
        tr ' ' '\n' |
        sed 's/\([0-9][0-9]*\.[0-9][0-9.]*\).*/\1/g' |
        grep -E '(^| )[0-9][0-9]*\.[0-9]' |
        head -n 1\`\"

    if test -z "$_ver"; then
        ## If empty, try date [fontforge]
        eval _ver=\"\`("$1" --version || "$1" -V) 2>&1 \
        | grep '\(^\|[^0-9a-f]\)[0-9]\{6,8\}\([^0-9a-f]\|$\)' \
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
# If not, add entry to missing-list ($3, one of 'OPTIONAL',
# 'REQUIRED') and assign "false" to ($1).  "false" can be tested
# clearly in makefiles and will surely fail if run unintentionally.
AC_DEFUN(STEPMAKE_OPTIONAL_REQUIRED, [
    STEPMAKE_CHECK_SEARCH_RESULT($1)
    if test $? -ne 0; then
        STEPMAKE_ADD_ENTRY($3, $2)
        eval "$1"=false
        false
    else
        true
    fi
])


# Return if tested proram ($1) was found (true) or not (false).
AC_DEFUN(STEPMAKE_CHECK_SEARCH_RESULT, [
    r="`eval echo '$'"$1"`"
    if test -n "$r" \
            -a "$r" != "error" \
            -a "$r" != "no" \
       && expr '`eval echo '$'"$1"`' : '.*\(echo\)' > /dev/null; then
        true
    else
        false
    fi
])

# Check version of program ($1)
# If version is smaller than requested ($3) or larger than requested
# ($4, optional), add entry to missing-list ($2, one of 'OPTIONAL',
# 'REQUIRED').
AC_DEFUN(STEPMAKE_CHECK_VERSION, [
    r="`eval echo '$'"$1"`"
    AC_MSG_CHECKING([$r version])
    exe=`STEPMAKE_GET_EXECUTABLE($r)`
    ver=`STEPMAKE_GET_VERSION($exe)`
    num=`STEPMAKE_NUMERIC_VERSION($ver)`
    min=`STEPMAKE_NUMERIC_VERSION($3)`
    AC_MSG_RESULT([$ver])
    if test "$num" -lt "$min"; then
        STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
    if test -n "$4"; then
        max=`STEPMAKE_NUMERIC_VERSION($4)`
        if test "$num" -gt "$max"; then
            STEPMAKE_ADD_ENTRY($2, ["$r <= $4 (installed: $ver)"])
        fi
    fi
    vervar="`echo $1 | tr '[a-z]' '[A-Z]'`_VERSION"
    eval `echo $vervar=$num`
##    AC_SUBST(`eval echo $vervar`)
])


### Macros to build configure.ac


AC_DEFUN(STEPMAKE_BISON, [
    STEPMAKE_PROGS(BISON, bison, $1)

    # urg.  should test functionality rather than version.
    if test "$BISON" = "bison" -a -n "$2"; then
        STEPMAKE_CHECK_VERSION(BISON, $1, $2)
    fi
])


AC_DEFUN(STEPMAKE_COMPILE_BEFORE, [
    # -O is necessary to get inlining
    CFLAGS=${CFLAGS-""}
    CXXFLAGS=${CXXFLAGS-$CFLAGS}
    LDFLAGS=${LDFLAGS-""}
    optimise_b=yes
    checks_b=no
    profile_b=no
    debug_b=yes
    pipe_b=yes

    AC_ARG_ENABLE(debugging,
        [AS_HELP_STRING(
             [--enable-debugging],
             [compile with debugging info.  Default: on])],
        [debug_b=$enableval])

    AC_ARG_ENABLE(checking,
        [AS_HELP_STRING(
             [--enable-checking],
             [compile with expensive run-time checks.  Default: off])],
        [checks_b=$enableval])

    AC_ARG_ENABLE(optimising,
        [AS_HELP_STRING(
            [--enable-optimising],
            [compile with optimising.  Default: on])],
        [optimise_b=$enableval])

    AC_ARG_ENABLE(profiling,
        [AS_HELP_STRING(
            [--enable-profiling],
            [compile with gprof support.  Default: off])],
        [profile_b=$enableval])

    AC_ARG_ENABLE(pipe,
        [AS_HELP_STRING(
            [--enable-pipe],
            [compile with -pipe.  Default: on])],
        [pipe_b=$enableval])

    if test "$optimise_b" = yes; then
        OPTIMIZE=" -O2 -finline-functions"
    fi

    if test "$checks_b" = yes; then
        DEFINES="$DEFINES -DDEBUG"
    fi

    if test $profile_b = yes; then
        EXTRA_LIBS="-pg"
        OPTIMIZE="$OPTIMIZE -pg"
    fi

    if test $debug_b = yes; then
        OPTIMIZE="$OPTIMIZE -g"
    fi
])


AC_DEFUN(STEPMAKE_COMPILE, [
    AC_REQUIRE([STEPMAKE_COMPILE_BEFORE])
    AC_REQUIRE([AC_PROG_CC])
    AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

    STEPMAKE_OPTIONAL_REQUIRED(CC, cc, $1)

    # If -pipe requested, test if it works and add to CFLAGS.
    if test "$pipe_b" = yes; then
        save_cflags="$CFLAGS"
        CFLAGS=" -pipe $CFLAGS";
        AC_CACHE_CHECK([whether compiler understands -pipe],
            [stepmake_cv_cflags_pipe],
            AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[/* -pipe test */]])],
                [stepmake_cv_cflags_pipe=yes],
                [stepmake_cv_cflags_pipe=no]))
        CFLAGS=$save_cflags
        if test $stepmake_cv_cflags_pipe = yes; then
            OPTIMIZE="$OPTIMIZE -pipe"
        fi
    fi

    CFLAGS="$CFLAGS $OPTIMIZE"
    CPPFLAGS=${CPPFLAGS-""}

    AC_SUBST(cross_compiling)
    AC_SUBST(CFLAGS)
    AC_SUBST(CPPFLAGS)
    AC_SUBST(LDFLAGS)
    AC_SUBST(DEFINES)
    AC_SUBST(EXTRA_LIBS)
])


AC_DEFUN(STEPMAKE_CXX, [
    AC_PROG_CXX
    STEPMAKE_OPTIONAL_REQUIRED(CXX, c++, $1)

    CXXFLAGS="$CXXFLAGS $OPTIMIZE"

    AC_SUBST(CXX)
    AC_SUBST(CXXFLAGS)
])


AC_DEFUN(STEPMAKE_DATADIR, [
    presome=${prefix}
    if test "$prefix" = "NONE"; then
        presome=${ac_default_prefix}
    fi

    build_package_datadir=$ac_pwd/out/share/$package

    DATADIR=`echo ${datadir} | sed "s!\\\${datarootdir}!${presome}/share!"`
    DATADIR=`echo ${DATADIR} | sed "s!\\\${prefix}!$presome!"`
    BUILD_PACKAGE_DATADIR=`echo ${build_package_datadir} | sed "s!\\\${prefix}!$presome!"`

    AC_SUBST(datadir)
    AC_SUBST(datarootdir)
    AC_SUBST(build_package_datadir)
    AC_DEFINE_UNQUOTED(CONFIG_DATADIR, ["${DATADIR}"])
    AC_DEFINE_UNQUOTED(BUILD_PACKAGE_DATADIR, ["${BUILD_PACKAGE_DATADIR}"])
])


## ugh: cut & paste programming from datadir.
AC_DEFUN(STEPMAKE_LIBDIR, [
    presome=${exec_prefix}
    if test "$presome" = "NONE"; then
        presome=${prefix}
    fi
    if test "$presome" = "NONE"; then
        presome=${ac_default_prefix}
    fi

    build_package_libdir=$ac_pwd/out/lib/$package

    LIBDIR=`echo ${libdir} | sed "s!\\\${exec_prefix}!$presome!"`
    BUILD_PACKAGE_LIBDIR=`echo ${build_package_libdir} | sed "s!\\\${exec_prefix}!$presome!"`

    AC_SUBST(libdir)
    AC_SUBST(build_package_libdir)
    AC_DEFINE_UNQUOTED(CONFIG_LIBDIR, ["${LIBDIR}"])
    AC_DEFINE_UNQUOTED(BUILD_PACKAGE_LIBDIR, ["${BUILD_PACKAGE_LIBDIR}"])
])


AC_DEFUN(STEPMAKE_PREFIX_EXPAND_FIXUP, [
    # undo expanding of explicit --infodir=/usr/share
    # to ease install-time override with prefix=...
    strip=`echo $includedir | eval sed s@^$prefix@@`
    if test "$includedir" = "`eval echo $prefix$strip`"; then
        includedir='${prefix}'$strip''
    fi
    strip=`echo $libdir | eval sed s@^$exec_prefix@@`
    if test "$libdir" = "`eval echo $exec_prefix$strip`"; then
        libdir='${exec_prefix}'$strip''
    fi
    strip=`echo $infodir | eval sed s@^$datarootdir@@`
    if test "$infodir" = "`eval echo $datarootdir$strip`"; then
        infodir='${datarootdir}'$strip''
    fi
    strip=`echo $mandir | eval sed s@^$datarootdir@@`
    if test "$mandir" = "`eval echo $datarootdir$strip`"; then
        mandir='${datarootdir}'$strip''
    fi
])


AC_DEFUN(STEPMAKE_END, [
    STEPMAKE_PREFIX_EXPAND_FIXUP

    AC_SUBST(OPTIONAL)
    AC_SUBST(REQUIRED)

    AC_CONFIG_FILES([config.make:config.make.in])
    AC_OUTPUT

    if test -n "$OPTIONAL"; then
        echo
        echo "WARNING: Please consider installing optional programs or files: $OPTIONAL"
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
    if test "$srcdir_build" != "yes"; then
        if test -f $srcdir/GNUmakefile; then
            cat <<EOF
Source directory already configured.  Please clean the source directory

    make -C $srcdir distclean

and rerun configure.
EOF
            exit 2
        fi

        depth=""
        for d in 2 3 4 5 ; do
            depth="$depth../"
            for mf in `cd $srcdir; \
                       find . -maxdepth $d -mindepth $d -name GNUmakefile`; do
                case "$abssrcdir" in
                "$absbuilddir"/*)
                    # source is below build directory, always copy
                    ;;
                *)
                    case "$abssrcdir/${mf#./}" in
                    "$absbuilddir"/*)
                        # find descended into build directory, don't copy
                        continue
                    esac
                esac

                mkdir -p ${mf%/*}
                cat <<EOF > $mf
depth=$depth
include \$(depth)/config.make
include \$(configure-srcdir)/$mf
EOF
            done

            for mf in `cd $srcdir; \
                       find . -maxdepth $d -mindepth $d -name '*.make' \
                       | grep -v config.make `; do
                case "$abssrcdir" in
                "$absbuilddir"/*)
                    # source is below build directory, always copy
                    ;;
                *)
                    case "$abssrcdir/${mf#./}" in
                    "$absbuilddir"/*)
                        # find descended into build directory, don't copy
                        continue
                    esac
                esac

                mkdir -p ${mf%/*}
                cat <<EOF > $mf
include \$(depth)/config.make
include \$(configure-srcdir)/$mf
EOF
            done
        done
    fi
    rm -f GNUmakefile
    cat <<EOF > GNUmakefile
depth = .
include config.make
include \$(configure-srcdir)/GNUmakefile.in
EOF
    chmod 444 GNUmakefile
    AC_SUBST(VPATH)
])


AC_DEFUN(STEPMAKE_FLEX, [
    # ugh, ugh
    ac_cv_prog_lex_root=lex.yy
    STEPMAKE_PROGS(FLEX, flex, $1)
])


AC_DEFUN(STEPMAKE_FLEXLEXER, [
    save_CPPFLAGS="$CPPFLAGS"
    if test -n "$FLEXLEXER_DIR"; then
        FLEXLEXER_CPPFLAGS="-I$FLEXLEXER_DIR"
        AC_SUBST(FLEXLEXER_CPPFLAGS)
        CPPFLAGS="$FLEXLEXER_CPPFLAGS $CPPFLAGS"
    fi
    AC_CHECK_HEADERS([FlexLexer.h],[true],[false])
    if test $? -ne 0; then
        warn='FlexLexer.h (flex package)'
        STEPMAKE_ADD_ENTRY($1, $warn)
    fi
    # check for yyFlexLexer.yypop_buffer_state () since flex 2.5.29
    AC_CACHE_CHECK([for yyFlexLexer.yypop_buffer_state ()],
        [stepmake_cv_flexlexer_yypop_buffer_state],
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM([[
#include <FlexLexer.h>
class yy_flex_lexer: public yyFlexLexer
{
  public:
    yy_flex_lexer ()
    {
      yypop_buffer_state ();
    }
};

            ]])],
            [stepmake_cv_flexlexer_yypop_buffer_state=yes],
            [stepmake_cv_flexlexer_yypop_buffer_state=no]))

    if test $stepmake_cv_flexlexer_yypop_buffer_state = no; then
        warn='FlexLexer.h with yypop_buffer_state (flex >= 2.5.29)'
        STEPMAKE_ADD_ENTRY($1, $warn)
    fi
    CPPFLAGS=$save_CPPFLAGS
])


AC_DEFUN(STEPMAKE_GETTEXT, [
    presome=${prefix}
    if test "$prefix" = "NONE"; then
        presome=${ac_default_prefix}
    fi
    LOCALEDIR=`echo ${localedir} | sed "s!\\\${prefix}!$presome!"`

    AC_SUBST(localedir)
    AC_DEFINE_UNQUOTED(LOCALEDIR, ["${LOCALEDIR}"])
    AC_CHECK_LIB(intl, gettext)
    AC_CHECK_FUNCS(gettext)
])


AC_DEFUN(STEPMAKE_GUILE_DEVEL, [
    AC_ARG_VAR(GUILE_FLAVOR,
               AS_HELP_STRING([], [pkgconfig name for Guile, like guile-2.2.
                                   If the respective .pc file
                                   cannot be found by pkgconfig,
                                   add its path to PKG_CONFIG_PATH]))dnl

    if test -n "$GUILE_FLAVOR"; then
        PKG_CHECK_MODULES([GUILE], [$GUILE_FLAVOR],
                            [true], [GUILE_FLAVOR="missing"])
    else
        PKG_CHECK_MODULES([GUILE], [guile-2.2 >= 2.2.0],
                            [GUILE_FLAVOR="guile-2.2"], [
            GUILE_FLAVOR="missing"
        ])
    fi

    if test "$GUILE_FLAVOR" = "missing"; then
        STEPMAKE_ADD_ENTRY(REQUIRED, ["guile-devel >= 2.2"])
    fi
])


AC_DEFUN(STEPMAKE_INIT, [
    . $srcdir/VERSION
    FULL_VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL
    TOPLEVEL_VERSION=$FULL_VERSION
    if test x$MY_PATCH_LEVEL != x; then
        FULL_VERSION=$FULL_VERSION.$MY_PATCH_LEVEL
    fi
    VERSION=$FULL_VERSION
    export MAJOR_VERSION MINOR_VERSION PATCH_LEVEL

    # urg, how is this supposed to work?
    if test "$program_prefix" = "NONE"; then
        program_prefix=
    fi
    if test "$program_suffix" = "NONE"; then
        program_suffix=
    fi

    # From configure: "When building in place, set srcdir=."
    if test "$srcdir" = "."; then
        srcdir_build=yes
    else
        srcdir_build=no
    fi
    abssrcdir="`cd $srcdir; pwd`"
    absbuilddir="`pwd`"


    AC_SUBST(VERSION)
    AC_SUBST(MAJOR_VERSION)
    AC_SUBST(MINOR_VERSION)

    # stepmake nonstandard names
    AC_SUBST(PATCH_LEVEL)
    AC_SUBST(TOPLEVEL_VERSION)

    AUTOGENERATE="This file was automatically generated by configure"
    AC_SUBST(AUTOGENERATE)

    AC_CANONICAL_HOST
    STEPMAKE_PROGS(MAKE, gmake make, REQUIRED)
    STEPMAKE_PROGS(FIND, find, REQUIRED)

    STEPMAKE_PROGS(TAR, tar, REQUIRED)

    STEPMAKE_PYTHON(REQUIRED, 3.6, 3.99)

    if expr "$MAKE" : '.*\(echo\)' >/dev/null; then
        $MAKE -v 2> /dev/null | grep GNU > /dev/null
        if test "$?" = 1; then
            warn='make (Please install *GNU* make)'
            STEPMAKE_ADD_ENTRY(REQUIRED, $warn)
        fi
    fi

    ROOTSEP=':'
    DIRSEP='/'
    PATHSEP=':'
    LN=ln
    LN_S='ln -s'

    AC_SUBST(program_prefix)
    AC_SUBST(program_suffix)
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


AC_DEFUN(STEPMAKE_LIB, [
    STEPMAKE_PROGS(AR, ar, $1)
    AC_PROG_RANLIB
    STEPMAKE_OPTIONAL_REQUIRED(RANLIB, ranlib, $1)
])


AC_DEFUN(STEPMAKE_LOCALE, [
    # with/enable ??
    AC_ARG_WITH(localedir,
        [AS_HELP_STRING(
             [--with-localedir=DIR],
             [location of locales.  Default: PREFIX/share/locale])],
        localedir=$with_localedir,
        localedir='${prefix}/share/locale')
])


AC_DEFUN(STEPMAKE_MSGFMT, [
    STEPMAKE_PROGS(MSGFMT, msgfmt, $1)
])


# Check for program ($2).  If found, assign full path result to ($1).
# If missing, add entry to missing-list ($3, one of 'OPTIONAL', 'REQUIRED')
# and assign "false" to ($1).
AC_DEFUN(STEPMAKE_PATH_PROG, [
    AC_CHECK_PROGS($1, $2, no)
    STEPMAKE_OPTIONAL_REQUIRED($1, $2, $3)
    if test $? -eq 0; then
        AC_PATH_PROGS($1, $2)
        if test -n "$4"; then
            STEPMAKE_CHECK_VERSION($1, $3, $4)
        fi
    fi
])


# Check for program in a set of names ($2).  If found, assign result to ($1).
# If missing, add entry to missing-list ($3, one of 'OPTIONAL', 'REQUIRED')
# and assign "false" to ($1).
# Otherwise, compare version to minimum version ($4, optional) and/or maximum
# version ($5, optional).
AC_DEFUN(STEPMAKE_PROGS, [
    AC_CHECK_PROGS($1, $2, no)
    STEPMAKE_OPTIONAL_REQUIRED($1, $2, $3)
    if test $? -eq 0; then
        if test -n "$4" -o -n "$5"; then
            STEPMAKE_CHECK_VERSION($1, $3, $4, $5)
        fi
    fi
])


AC_DEFUN(STEPMAKE_PERL, [
    STEPMAKE_PATH_PROG(PERL, perl, $1)
])


# Check for python, between minimum ($2) and maximum version ($3).
# If missing, add entry to missing-list ($1, one of 'OPTIONAL', 'REQUIRED')
AC_DEFUN(STEPMAKE_PYTHON, [
    AC_MSG_CHECKING([for python])
    python="python"
    found="no"
    for r in $PYTHON python python3 \
             python3.10 \
             python3.9 \
             python3.8 \
             python3.7 \
             python3.6; do
        exe=`STEPMAKE_GET_EXECUTABLE($r)`
        if ! $exe -V > /dev/null 2>&1 ; then
            continue
        fi
        ver=`STEPMAKE_GET_VERSION($exe)`
        num=`STEPMAKE_NUMERIC_VERSION($ver)`
        req=`STEPMAKE_NUMERIC_VERSION($2)`
        sup=`STEPMAKE_NUMERIC_VERSION($3)`
        if test -n "$2" && test "$num" -lt "$req"; then
            python=["$r >= $2 (installed: $ver)"]
            continue
        else
            if test -n "$3" && test "$num" -ge "$sup"; then
                python=["$r < $3 (installed: $ver)"]
                continue
            else
                python=$r
                found=$r
                break
            fi
        fi
    done
    AC_MSG_RESULT([$found])
    if test "$found" != "no"; then
        AC_MSG_CHECKING([$python version])
        AC_MSG_RESULT([$ver])
        PYTHON=$found
    else
        STEPMAKE_ADD_ENTRY($1, $python)
    fi
    AC_PATH_PROG(PYTHON, $PYTHON)
    AC_SUBST(PYTHON)
])


AC_DEFUN(STEPMAKE_FONTFORGE, [
    STEPMAKE_PATH_PROG(FONTFORGE, fontforge, $1, 20110222)
    if test $? -eq 0; then
        min_quiet=`STEPMAKE_NUMERIC_VERSION(20220308)`
        # `num` comes from `STEPMAKE_PATH_PROG`; we compare it with the
        # first version that gives the `-quiet` option the expected
        # functionality, that is, suppressing version and copyright
        # information sent to stderr.
        if test "$num" -ge "$min_quiet"; then
            FONTFORGE_QUIET_OPTION=-quiet
            AC_SUBST(FONTFORGE_QUIET_OPTION)
        fi
    fi
])


AC_DEFUN(STEPMAKE_TEXMF_DIRS, [
    STEPMAKE_PROGS(KPSEWHICH, kpsewhich, $1)

    AC_MSG_CHECKING(for metapost required files)
    if test "$MFPLAIN_MP" = ""; then
        MFPLAIN_MP=`kpsewhich -format=mp mfplain`
    fi
    if test "$MFPLAIN_MP" = ""; then
        AC_MSG_RESULT(no)
        STEPMAKE_ADD_ENTRY($1,
            ['metapost CTAN package (texlive-metapost)'])
    else
        AC_MSG_RESULT(yes)
    fi
])


AC_DEFUN(STEPMAKE_TEXMF, [
    STEPMAKE_PROGS(METAFONT, mf-nowin mf mfw mfont, $1)
    STEPMAKE_PROGS(METAPOST, mpost, $1)
    if test "$METAPOST" != ""; then
        ver=`STEPMAKE_GET_VERSION($METAPOST)`
        num=`STEPMAKE_NUMERIC_VERSION($ver)`
        # Avoid buggy metapost versions: 1.600 <= x < 1.803
        if test "$num" -ge "1600000" \
                -a "$num" -lt "1803000"; then
            STEPMAKE_ADD_ENTRY($1,
                ["mpost (due to a bug in metapost, versions 1.600 <= x < 1.803 are not supported; installed: $ver)"])
        fi
    fi

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


AC_DEFUN(STEPMAKE_WINDOWS, [
    AC_MSG_CHECKING([for some flavor of Windows])
    AC_CANONICAL_HOST
    case $host_os in
      *cygwin* )
        LN_S='cp -r' # Cygwin symbolic links do not work for native apps.
        program_suffix=.exe
        PLATFORM_WINDOWS=yes
        ;;
      *mingw32* )
        LN='cp -r'
        LN_S='cp -r'
        program_suffix=.exe
        PATHSEP=';'
        PLATFORM_WINDOWS=yes
        ;;
      * )
        PLATFORM_WINDOWS=no
        ;;
    esac
    AC_MSG_RESULT([$PLATFORM_WINDOWS])

    AC_SUBST(LN)
    AC_SUBST(LN_S)
    AC_DEFINE_UNQUOTED(DIRSEP, ['${DIRSEP}'])
    AC_DEFINE_UNQUOTED(PATHSEP, ['${PATHSEP}'])
    AC_SUBST(DIRSEP)
    AC_SUBST(PATHSEP)
    AC_SUBST(program_suffix)

    AC_SUBST(PLATFORM_WINDOWS)
    STEPMAKE_PROGS(WINDRES, $host-windres windres, x)
    AC_SUBST(WINDRES)
])

m4_include([m4/pkg.m4])
