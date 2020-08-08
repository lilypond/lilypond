dnl aclocal.m4   -*-shell-script-*-
dnl StepMake subroutines for configure.ac

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
        ##STEPMAKE_WARN(cannot find $2. $3)
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
        # following two lines are compatibility while Patchy has not
        # yet learnt about --enable-checking.  But once it has, we
        # don't want -DDEBUG twice, so we omit it here if it is going
        # to get added anyway later.
    elif test "$checks_b" != yes; then
        DEFINES="$DEFINES -DDEBUG"
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


AC_DEFUN(STEPMAKE_PREFIX_EXPAND_FIXUP, [
    # undo expanding of explicit --infodir=/usr/share
    # to ease install-time override with prefix=...
    strip=`echo $includedir | eval sed s@^$prefix@@`
    if test "$includedir" = "`eval echo $prefix$strip`"; then
        includedir='${prefix}'$strip''
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

    if test -n "$GUILE_CONFIG"; then
        echo
        echo "WARNING: GUILE_CONFIG deprecated, consider using PKG_CONFIG_PATH instead"
    fi

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
    # ugh, automake: we want (and check for) flex
    # AC_PROG_LEX
    # urg: automake 1.3: hope this doesn't break 1.2 ac_cv_pro_lex_root hack...

    # AC_PROG_LEX()
    # ugh, ugh
    ac_cv_prog_lex_root=lex.yy
    STEPMAKE_PROGS(FLEX, flex, $1)
])


AC_DEFUN(STEPMAKE_FLEXLEXER, [
    save_CPPFLAGS="$CPPFLAGS"
    if test -n "$FLEXLEXER_DIR"; then
        CPPFLAGS="-I$FLEXLEXER_DIR $CPPFLAGS"
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


AC_DEFUN(STEPMAKE_FLEXLEXER_LOCATION, [
    AC_MSG_CHECKING([FlexLexer.h location])

    save_CPPFLAGS="$CPPFLAGS"
    if test -n "$FLEXLEXER_DIR"; then
        CPPFLAGS="-I$FLEXLEXER_DIR $CPPFLAGS"
    fi

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
    if test -n "$FLEXLEXER_DIR"; then
        case $FLEXLEXER_FILE in
        */*)
            dir=${FLEXLEXER_FILE%/*}
            ;;
        *)
            dir=.
            ;;
        esac
        if test "x$dir" != "x$FLEXLEXER_DIR"; then
            AC_MSG_ERROR([FlexLexer.h not located in directory given by --with-flexlexer-dir])
        fi
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


# Check for guile, between minimum ($2) and maximum version ($3).
# If missing, add entry to missing-list ($1, one of 'OPTIONAL', 'REQUIRED')
AC_DEFUN(STEPMAKE_GUILE, [
    AC_MSG_CHECKING([for guile])
    guile="guile"
    found="no"
    for r in $GUILE guile guile2 guile2.2 guile2.0 guile-2.0 \
             guile1 guile19 guile18 \
             guile1.9 guile1.8 \
             guile-1 guile-1.9 guile-1.8; do
        exe=`STEPMAKE_GET_EXECUTABLE($r)`
        if ! $exe --version > /dev/null 2>&1 ; then
            continue
        fi
        ver=`STEPMAKE_GET_VERSION($exe)`
        num=`STEPMAKE_NUMERIC_VERSION($ver)`
        req=`STEPMAKE_NUMERIC_VERSION($2)`
        sup=`STEPMAKE_NUMERIC_VERSION($3)`
        if test -n "$2" && test "$num" -lt "$req"; then
            guile=["$r >= $2 (installed: $ver)"]
            continue
        else
            if test -n "$3" && test "$num" -ge "$sup"; then
                guile=["$r < $3 (installed: $ver)"]
                continue
            else
                guile=$r
                found=$r
                break
            fi
        fi
    done
    AC_MSG_RESULT([$found])
    if test "$found" != "no"; then
        AC_MSG_CHECKING([$guile version])
        AC_MSG_RESULT([$ver])
        GUILE=$found
    else
        STEPMAKE_ADD_ENTRY($1, $guile)
    fi
    STEPMAKE_PATH_PROG(GUILE, $GUILE)
])

AC_DEFUN(STEPMAKE_GUILE_DEVEL, [
    AC_ARG_VAR(GUILE_FLAVOR,
               AS_HELP_STRING([], [pkgconfig name for Guile, like guile-1.8 or
                                   guile-2.2 .  If the respective .pc file
                                   cannot be found by pkgconfig,
                                   add its path to PKG_CONFIG_PATH]))dnl
    AC_ARG_VAR(GUILE_CONFIG,
               [guile-config executable, obsoleted by pkgconfig/GUILE_FLAVOR])dnl

    if test -n "$GUILE_CONFIG"; then
        AC_MSG_CHECKING([GUILE_CONFIG for GUILE_FLAVOR])
        tmp_GUILE_FLAVOR="$($GUILE_CONFIG info guileversion)"
        case $tmp_GUILE_FLAVOR in
            *.*.*) # not a regexp, just a crude check
                GUILE_FLAVOR=guile-"${tmp_GUILE_FLAVOR%.*}"
                AC_MSG_RESULT([$GUILE_FLAVOR])
                ;;
            *)
                AC_MSG_ERROR([\$GUILE_CONFIG info guileversion failed: $tmp_GUILE_FLAVOR])
                ;;
        esac
        if test -z "$GUILE_CFLAGS"; then
            AC_MSG_CHECKING([GUILE_CONFIG for GUILE_CFLAGS])
            GUILE_CFLAGS=$($GUILE_CONFIG compile)
            AC_MSG_RESULT([$GUILE_CFLAGS])
        fi
        if test -z "$GUILE_LIBS"; then
            AC_MSG_CHECKING([GUILE_CONFIG for GUILE_LIBS])
            GUILE_LIBS=$($GUILE_CONFIG link)
            AC_MSG_RESULT([$GUILE_LIBS])
        fi
    elif test -n "$GUILE_FLAVOR"; then
        PKG_CHECK_MODULES([GUILE], [$GUILE_FLAVOR],
                            [true], [GUILE_FLAVOR="missing"])
    else
        PKG_CHECK_MODULES([GUILE], [guile-1.8 >= 1.8.2],
                            [GUILE_FLAVOR="guile-1.8"], [
            AC_MSG_RESULT([no])
            PKG_CHECK_MODULES([GUILE], [guile-2.2 >= 2.2.0],
                                [GUILE_FLAVOR="guile-2.2"], [
                AC_MSG_RESULT([no])
                PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.7],
                                    [GUILE_FLAVOR="guile-2.0"], [
                    AC_MSG_RESULT([no])
                    GUILE_FLAVOR="missing"])
            ])
        ])
    fi

    case "$GUILE_FLAVOR" in
        guile-2.0|guile-2.2|guile-3.0)
            GUILEv2=yes
            ;;
        guile-1.8)
            ;;
        *)
            STEPMAKE_ADD_ENTRY(REQUIRED, ["guile-devel >= 1.8"])
            ;;
    esac
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

    STEPMAKE_PYTHON(REQUIRED, 3.5, 3.99)

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


AC_DEFUN(STEPMAKE_MAKEINFO, [
    STEPMAKE_PROGS(MAKEINFO, makeinfo, $1)
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
             python3.8 \
             python3.7 \
             python3.6 \
             python3.5; do
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


AC_DEFUN(STEPMAKE_WARN, [
    AC_MSG_WARN($1)
    warn_b=yes
])


AC_DEFUN(STEPMAKE_GLIB, [
    PKG_CHECK_MODULES(GLIB, $1 >= $3, have_glib=yes, true)
    if test "$have_glib" = yes; then
        AC_DEFINE(HAVE_GLIB)
        AC_SUBST(GLIB_CFLAGS)
        AC_SUBST(GLIB_LIBS)
    else
        r="libglib-dev or glib?-devel"
        ver="`$PKG_CONFIG --modversion $1`"
        STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_GOBJECT, [
    PKG_CHECK_MODULES(GOBJECT, $1 >= $3, have_gobject=yes, true)
    if test "$have_gobject" = yes; then
        AC_DEFINE(HAVE_GOBJECT)
        AC_SUBST(GOBJECT_CFLAGS)
        AC_SUBST(GOBJECT_LIBS)
    else
        r="libgobject-dev or gobject?-devel"
        ver="`$PKG_CONFIG --modversion $1`"
        STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])


AC_DEFUN(STEPMAKE_FREETYPE2, [
    PKG_CHECK_MODULES(FREETYPE2, $1 >= $3, have_freetype2=yes, true)
    if test "$have_freetype2" = yes; then
        AC_DEFINE(HAVE_FREETYPE2)
        AC_SUBST(FREETYPE2_CFLAGS)
        AC_SUBST(FREETYPE2_LIBS)
    else
        # URG
        #r="lib$1-dev or $1-devel"
        r="libfreetype6-dev or freetype?-devel"
        ver="`$PKG_CONFIG --modversion $1`"
        STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])

AC_DEFUN(STEPMAKE_PANGO_FT2, [
    PKG_CHECK_MODULES(PANGO_FT2, $1 >= $3, have_pangoft2=yes, true)
    if test "$have_pangoft2" = yes ; then
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
        # URG
        #r="lib$1-dev or $1-devel"e
        r="libpango1.0-dev or pango?-devel"
        ver="`$PKG_CONFIG --modversion $1`"
        STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (installed: $ver)"])
    fi
])


AC_DEFUN(STEPMAKE_PANGO_FT2_WITH_OTF_FEATURE, [
    PKG_CHECK_MODULES(PANGO_FT2, $1 >= $3,
        have_pangoft2_with_otf_feature=yes, true)
    if test "$have_pangoft2_with_otf_feature" = yes; then
        AC_DEFINE(HAVE_PANGO_FT2_WITH_OTF_FEATURE)
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
        # URG
        #r="lib$1-dev or $1-devel"e
        r="libpango1.0-dev or pango?-devel"
        ver="`$PKG_CONFIG --modversion $1`"
        STEPMAKE_ADD_ENTRY($2, ["$r >= $3 (It is required if you'd like "])
        STEPMAKE_ADD_ENTRY($2, ["to use OpenType font feature. "])
        STEPMAKE_ADD_ENTRY($2, ["installed: $ver)"])
    fi
])


AC_DEFUN(STEPMAKE_FONTCONFIG, [
    PKG_CHECK_MODULES(FONTCONFIG, $1 >= $3, have_fontconfig=yes, true)
    if test "$have_fontconfig" = yes; then
        AC_DEFINE(HAVE_FONTCONFIG)
        AC_SUBST(FONTCONFIG_CFLAGS)
        AC_SUBST(FONTCONFIG_LIBS)
    else
        r="lib$1-dev or $1-devel"
        ver="`$PKG_CONFIG --modversion $1`"
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
    STEPMAKE_PROGS(WINDRES, $target-windres $host-windres windres, x)
    AC_SUBST(WINDRES)
])

# pkg.m4 - Macros to locate and utilise pkg-config.   -*- Autoconf -*-
# serial 12 (pkg-config-0.29.2)

dnl Copyright © 2004 Scott James Remnant <scott@netsplit.com>.
dnl Copyright © 2012-2015 Dan Nicholson <dbn.lists@gmail.com>
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.
dnl
dnl As a special exception to the GNU General Public License, if you
dnl distribute this file as part of a program that contains a
dnl configuration script generated by Autoconf, you may include it under
dnl the same distribution terms that you use for the rest of that
dnl program.

dnl PKG_PREREQ(MIN-VERSION)
dnl -----------------------
dnl Since: 0.29
dnl
dnl Verify that the version of the pkg-config macros are at least
dnl MIN-VERSION. Unlike PKG_PROG_PKG_CONFIG, which checks the user's
dnl installed version of pkg-config, this checks the developer's version
dnl of pkg.m4 when generating configure.
dnl
dnl To ensure that this macro is defined, also add:
dnl m4_ifndef([PKG_PREREQ],
dnl     [m4_fatal([must install pkg-config 0.29 or later before running autoconf/autogen])])
dnl
dnl See the "Since" comment for each macro you use to see what version
dnl of the macros you require.
m4_defun([PKG_PREREQ],
[m4_define([PKG_MACROS_VERSION], [0.29.2])
m4_if(m4_version_compare(PKG_MACROS_VERSION, [$1]), -1,
    [m4_fatal([pkg.m4 version $1 or higher is required but ]PKG_MACROS_VERSION[ found])])
])dnl PKG_PREREQ

dnl PKG_PROG_PKG_CONFIG([MIN-VERSION])
dnl ----------------------------------
dnl Since: 0.16
dnl
dnl Search for the pkg-config tool and set the PKG_CONFIG variable to
dnl first found in the path. Checks that the version of pkg-config found
dnl is at least MIN-VERSION. If MIN-VERSION is not specified, 0.9.0 is
dnl used since that's the first version where most current features of
dnl pkg-config existed.
AC_DEFUN([PKG_PROG_PKG_CONFIG],
[m4_pattern_forbid([^_?PKG_[A-Z_]+$])
m4_pattern_allow([^PKG_CONFIG(_(PATH|LIBDIR|SYSROOT_DIR|ALLOW_SYSTEM_(CFLAGS|LIBS)))?$])
m4_pattern_allow([^PKG_CONFIG_(DISABLE_UNINSTALLED|TOP_BUILD_DIR|DEBUG_SPEW)$])
AC_ARG_VAR([PKG_CONFIG], [path to pkg-config utility])
AC_ARG_VAR([PKG_CONFIG_PATH], [directories to add to pkg-config's search path])
AC_ARG_VAR([PKG_CONFIG_LIBDIR], [path overriding pkg-config's built-in search path])

if test "x$ac_cv_env_PKG_CONFIG_set" != "xset"; then
        AC_PATH_TOOL([PKG_CONFIG], [pkg-config])
fi
if test -n "$PKG_CONFIG"; then
        _pkg_min_version=m4_default([$1], [0.9.0])
        AC_MSG_CHECKING([pkg-config is at least version $_pkg_min_version])
        if $PKG_CONFIG --atleast-pkgconfig-version $_pkg_min_version; then
                AC_MSG_RESULT([yes])
        else
                AC_MSG_RESULT([no])
                PKG_CONFIG=""
        fi
fi[]dnl
])dnl PKG_PROG_PKG_CONFIG

dnl PKG_CHECK_EXISTS(MODULES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
dnl -------------------------------------------------------------------
dnl Since: 0.18
dnl
dnl Check to see whether a particular set of modules exists. Similar to
dnl PKG_CHECK_MODULES(), but does not set variables or print errors.
dnl
dnl Please remember that m4 expands AC_REQUIRE([PKG_PROG_PKG_CONFIG])
dnl only at the first occurence in configure.ac, so if the first place
dnl it's called might be skipped (such as if it is within an "if", you
dnl have to call PKG_CHECK_EXISTS manually
AC_DEFUN([PKG_CHECK_EXISTS],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
if test -n "$PKG_CONFIG" && \
    AC_RUN_LOG([$PKG_CONFIG --exists --print-errors "$1"]); then
  m4_default([$2], [:])
m4_ifvaln([$3], [else
  $3])dnl
fi])

dnl _PKG_CONFIG([VARIABLE], [COMMAND], [MODULES])
dnl ---------------------------------------------
dnl Internal wrapper calling pkg-config via PKG_CONFIG and setting
dnl pkg_failed based on the result.
m4_define([_PKG_CONFIG],
[if test -n "$$1"; then
    pkg_cv_[]$1="$$1"
 elif test -n "$PKG_CONFIG"; then
    PKG_CHECK_EXISTS([$3],
                     [pkg_cv_[]$1=`$PKG_CONFIG --[]$2 "$3" 2>/dev/null`
                      test "x$?" != "x0" && pkg_failed=yes ],
                     [pkg_failed=yes])
 else
    pkg_failed=untried
fi[]dnl
])dnl _PKG_CONFIG

dnl _PKG_SHORT_ERRORS_SUPPORTED
dnl ---------------------------
dnl Internal check to see if pkg-config supports short errors.
AC_DEFUN([_PKG_SHORT_ERRORS_SUPPORTED],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])
if $PKG_CONFIG --atleast-pkgconfig-version 0.20; then
        _pkg_short_errors_supported=yes
else
        _pkg_short_errors_supported=no
fi[]dnl
])dnl _PKG_SHORT_ERRORS_SUPPORTED


dnl PKG_CHECK_MODULES(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
dnl   [ACTION-IF-NOT-FOUND])
dnl --------------------------------------------------------------
dnl Since: 0.4.0
dnl
dnl Note that if there is a possibility the first call to
dnl PKG_CHECK_MODULES might not happen, you should be sure to include an
dnl explicit call to PKG_PROG_PKG_CONFIG in your configure.ac
AC_DEFUN([PKG_CHECK_MODULES],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1][_CFLAGS], [C compiler flags for $1, overriding pkg-config])dnl
AC_ARG_VAR([$1][_LIBS], [linker flags for $1, overriding pkg-config])dnl

pkg_failed=no
AC_MSG_CHECKING([for $2])

_PKG_CONFIG([$1][_CFLAGS], [cflags], [$2])
_PKG_CONFIG([$1][_LIBS], [libs], [$2])

m4_define([_PKG_TEXT], [Alternatively, you may set the environment variables $1[]_CFLAGS
and $1[]_LIBS to avoid the need to call pkg-config.
See the pkg-config man page for more details.])

if test $pkg_failed = yes; then
        AC_MSG_RESULT([no])
        _PKG_SHORT_ERRORS_SUPPORTED
        if test $_pkg_short_errors_supported = yes; then
                $1[]_PKG_ERRORS=`$PKG_CONFIG --short-errors --print-errors --cflags --libs "$2" 2>&1`
        else
                $1[]_PKG_ERRORS=`$PKG_CONFIG --print-errors --cflags --libs "$2" 2>&1`
        fi
        # Put the nasty error message in config.log where it belongs
        echo "$$1[]_PKG_ERRORS" >&AS_MESSAGE_LOG_FD

        m4_default([$4], [AC_MSG_ERROR(
[Package requirements ($2) were not met:

$$1_PKG_ERRORS

Consider adjusting the PKG_CONFIG_PATH environment variable if you
installed software in a non-standard prefix.

_PKG_TEXT])[]dnl
        ])
elif test $pkg_failed = untried; then
        AC_MSG_RESULT([no])
        m4_default([$4], [AC_MSG_FAILURE(
[The pkg-config script could not be found or is too old.  Make sure it
is in your PATH or set the PKG_CONFIG environment variable to the full
path to pkg-config.

_PKG_TEXT

To get pkg-config, see <http://pkg-config.freedesktop.org/>.])[]dnl
        ])
else
        $1[]_CFLAGS=$pkg_cv_[]$1[]_CFLAGS
        $1[]_LIBS=$pkg_cv_[]$1[]_LIBS
        AC_MSG_RESULT([yes])
        $3
fi[]dnl
])dnl PKG_CHECK_MODULES


dnl PKG_CHECK_MODULES_STATIC(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
dnl   [ACTION-IF-NOT-FOUND])
dnl ---------------------------------------------------------------------
dnl Since: 0.29
dnl
dnl Checks for existence of MODULES and gathers its build flags with
dnl static libraries enabled. Sets VARIABLE-PREFIX_CFLAGS from --cflags
dnl and VARIABLE-PREFIX_LIBS from --libs.
dnl
dnl Note that if there is a possibility the first call to
dnl PKG_CHECK_MODULES_STATIC might not happen, you should be sure to
dnl include an explicit call to PKG_PROG_PKG_CONFIG in your
dnl configure.ac.
AC_DEFUN([PKG_CHECK_MODULES_STATIC],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
_save_PKG_CONFIG=$PKG_CONFIG
PKG_CONFIG="$PKG_CONFIG --static"
PKG_CHECK_MODULES($@)
PKG_CONFIG=$_save_PKG_CONFIG[]dnl
])dnl PKG_CHECK_MODULES_STATIC


dnl PKG_INSTALLDIR([DIRECTORY])
dnl -------------------------
dnl Since: 0.27
dnl
dnl Substitutes the variable pkgconfigdir as the location where a module
dnl should install pkg-config .pc files. By default the directory is
dnl $libdir/pkgconfig, but the default can be changed by passing
dnl DIRECTORY. The user can override through the --with-pkgconfigdir
dnl parameter.
AC_DEFUN([PKG_INSTALLDIR],
[m4_pushdef([pkg_default], [m4_default([$1], ['${libdir}/pkgconfig'])])
m4_pushdef([pkg_description],
    [pkg-config installation directory @<:@]pkg_default[@:>@])
AC_ARG_WITH([pkgconfigdir],
    [AS_HELP_STRING([--with-pkgconfigdir], pkg_description)],,
    [with_pkgconfigdir=]pkg_default)
AC_SUBST([pkgconfigdir], [$with_pkgconfigdir])
m4_popdef([pkg_default])
m4_popdef([pkg_description])
])dnl PKG_INSTALLDIR


dnl PKG_NOARCH_INSTALLDIR([DIRECTORY])
dnl --------------------------------
dnl Since: 0.27
dnl
dnl Substitutes the variable noarch_pkgconfigdir as the location where a
dnl module should install arch-independent pkg-config .pc files. By
dnl default the directory is $datadir/pkgconfig, but the default can be
dnl changed by passing DIRECTORY. The user can override through the
dnl --with-noarch-pkgconfigdir parameter.
AC_DEFUN([PKG_NOARCH_INSTALLDIR],
[m4_pushdef([pkg_default], [m4_default([$1], ['${datadir}/pkgconfig'])])
m4_pushdef([pkg_description],
    [pkg-config arch-independent installation directory @<:@]pkg_default[@:>@])
AC_ARG_WITH([noarch-pkgconfigdir],
    [AS_HELP_STRING([--with-noarch-pkgconfigdir], pkg_description)],,
    [with_noarch_pkgconfigdir=]pkg_default)
AC_SUBST([noarch_pkgconfigdir], [$with_noarch_pkgconfigdir])
m4_popdef([pkg_default])
m4_popdef([pkg_description])
])dnl PKG_NOARCH_INSTALLDIR


dnl PKG_CHECK_VAR(VARIABLE, MODULE, CONFIG-VARIABLE,
dnl [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
dnl -------------------------------------------
dnl Since: 0.28
dnl
dnl Retrieves the value of the pkg-config variable for the given module.
AC_DEFUN([PKG_CHECK_VAR],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1], [value of $3 for $2, overriding pkg-config])dnl

_PKG_CONFIG([$1], [variable="][$3]["], [$2])
AS_VAR_COPY([$1], [pkg_cv_][$1])

AS_VAR_IF([$1], [""], [$5], [$4])dnl
])dnl PKG_CHECK_VAR
