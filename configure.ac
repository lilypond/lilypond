dnl configure.ac   -*-shell-script-*-
dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.60])

# Bootstrap the init process.
AC_INIT([LilyPond],
    [m4_esyscmd_s([. ${SRCDIR:-.}/VERSION; echo $MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL])],
    [bug-lilypond@gnu.org], [lilypond], [https://lilypond.org/])

# Bootstrap StepMake configure
AC_CONFIG_AUX_DIR([config])

STEPMAKE_INIT

# For avoiding x86 FPU precision issues
AS_CASE([$host_cpu],
  [i?86], [CXXFLAGS="$CXXFLAGS -msse2 -mfpmath=sse"]
)

# List a file that identifies your package.
AC_CONFIG_SRCDIR([lily/main.cc])

# Move to aclocal.m4?
AC_CONFIG_HEADERS([config.hh:config.hh.in])

# Uncomment the configuration options your package needs.

DOCUMENTATION=yes
AC_ARG_ENABLE(documentation,
    [AS_HELP_STRING([--enable-documentation],
                    [build Documentation.  Default: on])],
    [DOCUMENTATION=$enableval])
AC_SUBST(DOCUMENTATION)

ENABLE_TEXI2HTML=yes
AC_ARG_ENABLE(texi2html,
    [AS_HELP_STRING([--enable-texi2html],
                    [build texi2html for HTML documentation.  Default: on])],
    [ENABLE_TEXI2HTML=$enableval])

AC_ARG_WITH(extractpdfmark,
    [AS_HELP_STRING([--with-extractpdfmark],
                    [use extractpdfmark for PDF documentation.]
                    [Default: check])],
    [],
    [with_extractpdfmark=check])

AC_ARG_WITH(flexlexer-dir,
    [AS_HELP_STRING([--with-flexlexer-dir=DIR],
                    [location of FlexLexer.h.])],
    [FLEXLEXER_DIR=$withval],
    [FLEXLEXER_DIR=""])

LINK_GXX_STATICALLY=no
AC_ARG_ENABLE(static-gxx,
    [AS_HELP_STRING([--enable-static-gxx],
                    [link libstdc++.a statically.  Default: off])],
    [LINK_GXX_STATICALLY=$enableval])
AC_SUBST(LINK_GXX_STATICALLY)

# must come before any header checks
STEPMAKE_COMPILE

AC_LANG([C++])

STEPMAKE_PYTHON(REQUIRED, 3.6, 3.99)

STEPMAKE_CXX(REQUIRED)
STEPMAKE_LIB(REQUIRED)
STEPMAKE_BISON(REQUIRED, 2.4.1)
STEPMAKE_FLEX(REQUIRED)
STEPMAKE_FLEXLEXER(REQUIRED)
STEPMAKE_LOCALE
STEPMAKE_GETTEXT
STEPMAKE_MSGFMT(REQUIRED)
STEPMAKE_TEXMF(REQUIRED)
STEPMAKE_TEXMF_DIRS(REQUIRED)

STEPMAKE_GUILE_DEVEL(REQUIRED)

# Check if the compiler understands -Werror=unknown-warning-option.
# This is important for Clang which otherwise just warns about unknown
# options.  GCC doesn't need this option and always returns an error.
# Unfortunately GCC doesn't support Clang's option either, so we can't
# just add it for all cases.
AC_MSG_CHECKING([whether $CXX supports -Werror=unknown-warning-option])
save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="-Werror=unknown-warning-option"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
                  [AC_MSG_RESULT([yes])
                   test_CXXFLAGS="-Werror=unknown-warning-option"],
                  [AC_MSG_RESULT([no])
                   test_CXXFLAGS=""])
CXXFLAGS="$save_CXXFLAGS"

# -Wcast-function-type was added to GCC in version 8.  The Guile
# interface tends to trigger this warning, so disable it.
# g++ -Wno-whatever may compile even when the compiler does not
# support the warning, so we have to check for the warning itself.
AC_MSG_CHECKING([whether $CXX supports -Wcast-function-type])
save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$test_CXXFLAGS -Wcast-function-type"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
                  [AC_MSG_RESULT([yes])
                   CXXFLAGS="$save_CXXFLAGS -Wno-cast-function-type"],
                  [AC_MSG_RESULT([no])
                   CXXFLAGS="$save_CXXFLAGS"])

PKG_CHECK_MODULES(BDWGC, bdw-gc)
GUILE_CFLAGS="${GUILE_CFLAGS} ${BDWGC_CFLAGS}"

## Check for usable cxxabi
save_LIBS="$LIBS"
LIBS="$LIBS $CXXABI_LIBS"
AC_MSG_CHECKING(for usable C++ demangler)
AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <cxxabi.h>],
            [abi::__cxa_demangle ("", 0, 0, 0)])],
    [AC_DEFINE(HAVE_CXA_DEMANGLE)
        AC_MSG_RESULT(yes)],[
        LIBS="$LIBS -lsupc++"
        AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <cxxabi.h>],
                    [abi::__cxa_demangle ("", 0, 0, 0)])],
            [AC_DEFINE(HAVE_CXA_DEMANGLE)
                AC_MSG_RESULT([yes, using -lsupc++])
                CXXABI_LIBS="$CXXABI_LIBS -lsupc++"],
            [AC_MSG_RESULT([no, use c++filt -t for manual demangling])])])
LIBS="$save_LIBS"
AC_SUBST(CXXABI_LIBS)

STEPMAKE_FONTFORGE(REQUIRED)

STEPMAKE_PATH_PROG(T1ASM, t1asm, REQUIRED)

AC_CHECK_HEADERS([grp.h pwd.h sys/stat.h])
AC_CHECK_FUNCS([chroot gettext])

PKG_CHECK_MODULES(FONTCONFIG, fontconfig >= 2.4.0)
AC_SUBST(FONTCONFIG_CFLAGS)
AC_SUBST(FONTCONFIG_LIBS)

PKG_CHECK_MODULES(FREETYPE2, freetype2 >= 2.3.9)
AC_SUBST(FREETYPE2_CFLAGS)
AC_SUBST(FREETYPE2_LIBS)

PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.38)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

PKG_CHECK_MODULES(GOBJECT, gobject-2.0 >= 2.38)
AC_SUBST(GOBJECT_CFLAGS)
AC_SUBST(GOBJECT_LIBS)

PKG_CHECK_MODULES(PANGO_FT2, pangoft2 >= 1.40.0)
AC_SUBST(PANGO_FT2_CFLAGS)
AC_SUBST(PANGO_FT2_LIBS)

CAIRO_BACKEND=no
AC_ARG_ENABLE(cairo-backend,
    [AS_HELP_STRING([--enable-cairo-backend],
                    [enable the cairo output backend (-dbackend=cairo)]
                    [Default: off])],
    [CAIRO_BACKEND=$enableval])
AC_SUBST(CAIRO_BACKEND)

if test "$CAIRO_BACKEND" = "yes"; then
    PKG_CHECK_MODULES(CAIRO, cairo >= 1.8.8)
    AC_SUBST(CAIRO_CFLAGS)
    AC_SUBST(CAIRO_LIBS)
    PKG_CHECK_MODULES(LIBPNG, libpng >= 1.6.0)
    AC_SUBST(LIBPNG_CFLAGS)
    AC_SUBST(LIBPNG_LIBS)
    AC_DEFINE(CAIRO_BACKEND)
fi

STEPMAKE_WINDOWS

# perl for help2man and for mf2pt1.pl
STEPMAKE_PERL(REQUIRED)

## Optional tools for building documentation, website, extra fonts.
if test "$DOCUMENTATION" = "yes"; then
  DOCUMENTATION_REQUIRED=REQUIRED
else
  DOCUMENTATION_REQUIRED=OPTIONAL
fi

# GhostScript
STEPMAKE_PATH_PROG(GHOSTSCRIPT, gs, $DOCUMENTATION_REQUIRED, 9.03)

GS_API=no
AC_ARG_ENABLE(gs-api,
    [AS_HELP_STRING([--enable-gs-api],
                    [link to libgs and use Ghostscript API instead of invoking]
                    [the executable. Beware of licensing implications!]
                    [Default: off])],
    [GS_API=$enableval])
AC_SUBST(GS_API)

if test "$GS_API" = "yes"; then
    AC_CHECK_HEADER([ghostscript/iapi.h], [
        AC_CHECK_LIB([gs], [gsapi_new_instance], [GHOSTSCRIPT_FOUND="yes"],
                     [GHOSTSCRIPT_FOUND="no"])
    ], [GHOSTSCRIPT_FOUND="no"])
    if test "$GHOSTSCRIPT_FOUND" = "no"; then
        STEPMAKE_ADD_ENTRY(REQUIRED, ["libgs-dev"])
    fi
    AC_DEFINE(GS_API)
fi

AC_SUBST(GS920)
AC_SUBST(EXTRACTPDFMARK)
AC_SUBST(USE_EXTRACTPDFMARK)

if test "x$with_extractpdfmark" = "xyes"; then
    EXTRACTPDFMARK_REQUIRED=REQUIRED
else
    EXTRACTPDFMARK_REQUIRED=OPTIONAL
fi

if test "x$with_extractpdfmark" != "xno"; then
    USE_EXTRACTPDFMARK=yes
    STEPMAKE_PROGS(EXTRACTPDFMARK, extractpdfmark, $EXTRACTPDFMARK_REQUIRED)

    STEPMAKE_PROGS(GS920, gs, OPTIONAL, 9.20)
    req=`STEPMAKE_NUMERIC_VERSION(9.20)`
    if test "$GS920_VERSION" -lt "$req"; then
        GS920=
        USE_EXTRACTPDFMARK=no
    fi

    if test "$EXTRACTPDFMARK" = "false"; then
        EXTRACTPDFMARK=
        USE_EXTRACTPDFMARK=no
    fi

    if test "$EXTRACTPDFMARK_REQUIRED" = "OPTIONAL"; then
        if test -z "$EXTRACTPDFMARK" || test -z "GS920"; then
            STEPMAKE_ADD_ENTRY(OPTIONAL,
                ["(Using GhostScript >= 9.20 together with extractpdfmark"])
            STEPMAKE_ADD_ENTRY(OPTIONAL,
                [" can significantly reduce the size of the final PDF files.)"])
        fi
    fi
else
    USE_EXTRACTPDFMARK=no
fi

STEPMAKE_PROGS(MAKEINFO, makeinfo, $DOCUMENTATION_REQUIRED, 6.1)
TEXI2HTML_REQUIRED=$DOCUMENTATION_REQUIRED
if test "$ENABLE_TEXI2HTML" = "no"; then
  TEXI2HTML_REQUIRED=OPTIONAL
fi
AC_SUBST(TEXI2HTML_VERSION)
STEPMAKE_PROGS(TEXI2HTML, texi2html, $TEXI2HTML_REQUIRED, 1.82)
STEPMAKE_PROGS(DBLATEX, dblatex, OPTIONAL, 0.1.4)
STEPMAKE_PROGS(BIBTEX, bibtex, $DOCUMENTATION_REQUIRED)
STEPMAKE_PROGS(PDFLATEX, xelatex pdflatex, $DOCUMENTATION_REQUIRED)
if test "$PDFLATEX" = "xelatex"; then
    STEPMAKE_PROGS(PDFTEX, xetex, $DOCUMENTATION_REQUIRED)
    if test "$PDFTEX" = "xetex"; then
        # For avoiding PDF destination name replacement.
        PDFTEX=$abssrcdir/scripts/build/xetex-with-options.sh
        PDFLATEX=$abssrcdir/scripts/build/xelatex-with-options.sh
    fi
else
    STEPMAKE_PROGS(PDFTEX, pdfetex pdftex etex, $DOCUMENTATION_REQUIRED)
fi
STEPMAKE_PROGS(TEXI2PDF, texi2pdf, $DOCUMENTATION_REQUIRED)
STEPMAKE_PROGS(TEXINDEX, texindex, $DOCUMENTATION_REQUIRED)

AC_MSG_CHECKING(for epsf.tex)
TEX_EPSF=`kpsewhich -format=tex epsf`
if test "$TEX_EPSF" = ""; then
    AC_MSG_RESULT(not found)
    STEPMAKE_ADD_ENTRY($DOCUMENTATION_REQUIRED,epsf.tex)
else
    AC_MSG_RESULT($TEX_EPSF)
fi

STEPMAKE_PROGS(PLTOTF, pltotf, $DOCUMENTATION_REQUIRED)
AC_MSG_CHECKING(for TeX fontinst package support)
TEX_FONTINST=`kpsewhich -format=tex fontinst.sty`
if test "$TEX_FONTINST" = ""; then
    AC_MSG_RESULT(not found)
    STEPMAKE_ADD_ENTRY($DOCUMENTATION_REQUIRED,
['fontinst CTAN package (texlive-fontinst or texlive-font-utils)'])
else
    AC_MSG_RESULT(yes)
fi
AC_MSG_CHECKING(for lmodern Type1 fonts)
# We check for `lmtl10.afm` -- if this file is present, the Type1 fonts are
# certainly installed, too.
TEX_LM=`kpsewhich -format=afm lmtl10`
if test "$TEX_LM" = ""; then
    AC_MSG_RESULT(not found)
    STEPMAKE_ADD_ENTRY($DOCUMENTATION_REQUIRED,
['lm CTAN package (lmodern or texlive-lm)'])
else
    AC_MSG_RESULT(yes)
fi
AC_MSG_CHECKING(for Cyrillic characters support in TeX)
TEX_FIKPARM=`kpsewhich -format=mf fikparm`
if test "$TEX_FIKPARM" = ""; then
    AC_MSG_RESULT(not found)
    STEPMAKE_ADD_ENTRY($DOCUMENTATION_REQUIRED,
['lh CTAN package (texlive-lang-cyrillic or texlive-texmf-fonts)'])
else
    AC_MSG_RESULT(yes)
fi

STEPMAKE_PROGS(IMAGEMAGICK, convert, $DOCUMENTATION_REQUIRED)
STEPMAKE_PROGS(RSYNC, rsync, $DOCUMENTATION_REQUIRED)

# must also have local.make if configure fails.
touch local.make

# Gather requirements and generate output.
STEPMAKE_END

if make top-doc 1>/dev/null 2>&1; then
    rm -f INSTALL.txt && ln Documentation/out/topdocs/INSTALL.txt .
fi

cat <<EOF

Type:
    make all       to build LilyPond
    make install   to install LilyPond
    make help      to see all possible targets

Edit local.make for local Makefile overrides.

See INSTALL.txt and README.md for more information

EOF
