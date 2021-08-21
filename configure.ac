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

AC_ARG_WITH(texgyre-dir,
    [AS_HELP_STRING([--with-texgyre-dir=DIR],
                    [location of TeX Gyre fonts.])],
    [TEXGYRE_DIR=$withval],
    [TEXGYRE_DIR=""])

AC_ARG_WITH(urwotf-dir,
    [AS_HELP_STRING([--with-urwotf-dir=DIR],
                    [location of URW++ OTF fonts.])],
    [URWOTF_DIR=$withval],
    [URWOTF_DIR=""])

rpath_b=no
AC_ARG_ENABLE(rpath,
    [AS_HELP_STRING([--enable-rpath],
                    [hardcode runtime library path.  Default: off])],
    [rpath_b=$enableval])

LINK_GXX_STATICALLY=no
AC_ARG_ENABLE(static-gxx,
    [AS_HELP_STRING([--enable-static-gxx],
                    [link libstdc++.a statically.  Default: off])],
    [LINK_GXX_STATICALLY=$enableval])
AC_SUBST(LINK_GXX_STATICALLY)

# must come before any header checks
STEPMAKE_COMPILE

AC_CHECK_PROG(FCLIST, fc-list, fc-list)
AC_MSG_CHECKING([for TeX Gyre fonts OTF files])
AC_SUBST(TEXGYRE_DIR)
AC_SUBST(TEXGYRE_FILES)
if test "$TEXGYRE_DIR" = ""; then
  if test "$FCLIST" != ""; then
    TEXGYRE_FILE=`$FCLIST "TeX Gyre Schola:fontformat=CFF" file | head -n 1`
    if test "$TEXGYRE_FILE" != ""; then
      TEXGYRE_FILE=`echo $TEXGYRE_FILE | sed 's/\(:.*\)$//g'`
      TEXGYRE_FILE=`$PYTHON "$srcdir/scripts/auxiliar/readlink.py" $TEXGYRE_FILE`
      TEXGYRE_DIR=`AS_DIRNAME($TEXGYRE_FILE)`
    fi
  fi
fi
if test "$TEXGYRE_DIR" != "" ;  then
  for f in texgyreschola-regular.otf texgyreschola-italic.otf \
           texgyreschola-bold.otf texgyreschola-bolditalic.otf \
           texgyreheros-regular.otf texgyreheros-italic.otf \
           texgyreheros-bold.otf texgyreheros-bolditalic.otf \
           texgyrecursor-regular.otf texgyrecursor-italic.otf \
           texgyrecursor-bold.otf texgyrecursor-bolditalic.otf; do
    if test ! -f "$TEXGYRE_DIR/$f"; then
      MISSING_TEXGYRE_FILES="$MISSING_TEXGYRE_FILES $f"
    else
      TEXGYRE_FILES="$TEXGYRE_FILES $f"
    fi
  done
fi
if test "$TEXGYRE_DIR" = "" -o "$MISSING_TEXGYRE_FILES" != ""; then
  AC_MSG_RESULT(no)
  STEPMAKE_ADD_ENTRY(REQUIRED,TeX Gyre fonts OTF)
  if test "$TEXGYRE_DIR" = ""; then
    if test "$FCLIST" = ""; then
      STEPMAKE_ADD_ENTRY(REQUIRED,'(install the fc-list utility from FontConfig, or use --with-texgyre-dir)')
    else
      STEPMAKE_ADD_ENTRY(REQUIRED,"(make sure the fc-list utility can see them, e.g. 'sudo apt-get install fonts-texgyre', or use --with-texgyre-dir)")
    fi
  else
    STEPMAKE_ADD_ENTRY(REQUIRED,'(these files are missing: $MISSING_TEXGYRE_FILES)')
  fi
else
  AC_MSG_RESULT(yes)
fi
AC_MSG_CHECKING([for URW++ OTF files])
AC_SUBST(URWOTF_DIR)
AC_SUBST(URWOTF_FILES)
if test "$URWOTF_DIR" = ""; then
  if test "$FCLIST" != ""; then
    URWOTF_FILE=`$FCLIST "C059:fontformat=CFF" file | head -n 1`
    if test "$URWOTF_FILE" != ""; then
      URWOTF_FILE=`echo $URWOTF_FILE | sed 's/\(:.*\)$//g'`
      URWOTF_FILE=`$PYTHON "$srcdir/scripts/auxiliar/readlink.py" $URWOTF_FILE`
      URWOTF_DIR=`AS_DIRNAME($URWOTF_FILE)`
    fi
  fi
fi
if test "$URWOTF_DIR" != "" ;  then
  for f in C059-BdIta.otf C059-Bold.otf \
           C059-Italic.otf C059-Roman.otf \
           NimbusMonoPS-Bold.otf NimbusMonoPS-BoldItalic.otf \
           NimbusMonoPS-Italic.otf NimbusMonoPS-Regular.otf \
           NimbusSans-Bold.otf NimbusSans-BoldItalic.otf \
           NimbusSans-Italic.otf NimbusSans-Regular.otf; do
    if test ! -f "$URWOTF_DIR/$f"; then
      MISSING_URWOTF_FILES="$MISSING_URWOTF_FILES $f"
    else
      URWOTF_FILES="$URWOTF_FILES $f"
    fi
  done
fi
if test "$MISSING_URWOTF_FILES"="NimbusSans-BoldItalic.otf NimbusSans-Italic.otf" && \
   test -f "$URWOTF_DIR/NimbusSans-BoldOblique.otf" && \
   test -f "$URWOTF_DIR/NimbusSans-Oblique.otf"; then
  MISSING_URWOTF_FILES=""
  URWOTF_FILES="$URWOTF_FILES NimbusSans-BoldOblique.otf NimbusSans-Oblique.otf"
fi
if test "$URWOTF_DIR" = "" -o "$MISSING_URWOTF_FILES" != ""; then
  AC_MSG_RESULT(no)
  STEPMAKE_ADD_ENTRY(OPTIONAL,URW++ OTF fonts)
  if test "$URWOTF_DIR" = ""; then
    if test "$FCLIST" = ""; then
      STEPMAKE_ADD_ENTRY(OPTIONAL,'(install the fc-list utility from FontConfig, or use --with-urwotf-dir)')
    else
      STEPMAKE_ADD_ENTRY(OPTIONAL,"(download OTF files from 'http://git.ghostscript.com/?p=urw-core35-fonts.git;a=tree;hb=91edd6ece36e84a1c6d63a1cf63a1a6d84bd443a' and put them under '~/.local/share/fonts' etc., or use --with-urwotf-dir)")
    fi
  else
    STEPMAKE_ADD_ENTRY(OPTIONAL,'(these files are missing: $MISSING_URWOTF_FILES)')
  fi
else
  AC_MSG_RESULT(yes)
fi

AC_LANG([C++])

STEPMAKE_PYTHON(REQUIRED, 3.5, 3.99)

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

save_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$GUILE_CFLAGS $CPPFLAGS"
# Some distributions like MacPorts allow guile 1.8 to co-exist with
# guile >= 2; they rename 'libguile.h' to 'libguile18.h'.
AC_CHECK_HEADERS([libguile18.h])

# check for 3 typedefs added in Guile 1.9
AC_CHECK_TYPES([scm_t_hash_fold_fn],
               [AC_DEFINE(HAVE_GUILE_HASH_FUNC)], [],
               [[#if HAVE_LIBGUILE18_H
# include <libguile18.h>
#else
# include <libguile.h>
#endif]])
AC_CHECK_TYPES([scm_t_subr],
               [AC_DEFINE(HAVE_GUILE_SUBR_TYPE)], [],
               [[#if HAVE_LIBGUILE18_H
# include <libguile18.h>
#else
# include <libguile.h>
#endif]])
CPPFLAGS="$save_CPPFLAGS"

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

# -Wsequence-point in GCC version 7 has become very noisy for the
# Guile 1.8 interface, so disable it.
# g++ -Wno-whatever may compile even when the compiler does not
# support the warning, so we have to check for the warning itself.
if test "$GUILEv2" != "yes"; then
    AC_MSG_CHECKING([whether $CXX supports -Wsequence-point])
    save_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$test_CXXFLAGS -Wsequence-point"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
                      [AC_MSG_RESULT([yes])
                       CXXFLAGS="$save_CXXFLAGS -Wno-sequence-point"],
                      [AC_MSG_RESULT([no])
                       CXXFLAGS="$save_CXXFLAGS"])
fi

if test "$GUILEv2" = "yes"; then
    PKG_CHECK_MODULES(BDWGC, bdw-gc, true,
    STEPMAKE_ADD_ENTRY(REQUIRED, libgc-devel))
    GUILE_CFLAGS="${GUILE_CFLAGS} ${BDWGC_CFLAGS}"
fi

# GCC versions 5 and up support expecting the use of the C++11 keyword
# "override" when a derived class overrides a virtual method.  This
# optional keyword makes the code easier to understand.
AC_MSG_CHECKING([whether $CXX supports -Wsuggest-override])
save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$test_CXXFLAGS -Wsuggest-override"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
                  [AC_MSG_RESULT([yes])
                   CXXFLAGS="$save_CXXFLAGS -Wsuggest-override"],
                  [AC_MSG_RESULT([no])
                   CXXFLAGS="$save_CXXFLAGS"])

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

STEPMAKE_PATH_PROG(FONTFORGE, fontforge, REQUIRED, 20110222)

STEPMAKE_PATH_PROG(T1ASM, t1asm, REQUIRED)

AC_CHECK_HEADERS([assert.h grp.h libio.h pwd.h sys/stat.h wchar.h])
AC_HEADER_STAT
AC_FUNC_MEMCMP
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([chroot gettext])

STEPMAKE_PROGS(PKG_CONFIG, pkg-config, REQUIRED, 0.9.0)

AC_MSG_CHECKING(for rpath linkage)
if test "$rpath_b" = "yes"; then
    LDFLAGS="$LDFLAGS -Wl,-rpath -Wl,\\\$\$ORIGIN/../lib"
elif test "$rpath_b" != "no"; then
    LDFLAGS="$LDFLAGS -Wl,-rpath -Wl,$rpath_b"
fi
AC_MSG_RESULT($rpath_b)

HOST_ARCH=`$CC -dumpmachine`
AC_SUBST(HOST_ARCH)

STEPMAKE_PANGO_FT2_WITH_OTF_FEATURE(pangoft2, OPTIONAL, 1.38.0)
if test "$have_pangoft2_with_otf_feature" != yes ; then
    STEPMAKE_PANGO_FT2(pangoft2, REQUIRED, 1.36.0)
fi
STEPMAKE_FONTCONFIG(fontconfig, REQUIRED, 2.4.0)
STEPMAKE_FREETYPE2(freetype2, REQUIRED, 2.3.9)
STEPMAKE_GLIB(glib-2.0, REQUIRED, 2.38)
STEPMAKE_GOBJECT(gobject-2.0, REQUIRED, 2.38)

CAIRO_BACKEND=no
AC_ARG_ENABLE(cairo-backend,
    [AS_HELP_STRING([--enable-cairo-backend],
                    [enable the cairo output backend (-dbackend=cairo)]
                    [Default: off])],
    [CAIRO_BACKEND=$enableval])
AC_SUBST(CAIRO_BACKEND)

if test "$CAIRO_BACKEND" = "yes"; then
    STEPMAKE_CAIRO(cairo, REQUIRED, 1.8.8)
    STEPMAKE_LIBPNG(libpng, REQUIRED, 1.2.56)
    AC_DEFINE(CAIRO_BACKEND)
fi

STEPMAKE_WINDOWS

# guile executable for lilypond-invoke-editor. We don't care about the
# version, and it doesn't have to be sync'd with the main lilypond
# GUILE version
STEPMAKE_GUILE(REQUIRED, 1.8.2)

# perl for help2man and for mf2pt1.pl
STEPMAKE_PERL(REQUIRED)

# tidy can be used to validate the HTML pages produced during
# regression testing
STEPMAKE_PATH_PROG(TIDY, tidy, OPTIONAL)

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

if test "x$with_extractpdfmark" == "xyes"; then
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

    if test "$EXTRACTPDFMARK" == "false"; then
        EXTRACTPDFMARK=
        USE_EXTRACTPDFMARK=no
    fi

    if test "$EXTRACTPDFMARK_REQUIRED" == "OPTIONAL"; then
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
TEX_EPSF=`kpsewhich tex epsf`
if test "$TEX_EPSF" = ""; then
    AC_MSG_RESULT(not found)
    STEPMAKE_ADD_ENTRY($DOCUMENTATION_REQUIRED,epsf.tex)
else
    AC_MSG_RESULT($TEX_EPSF)
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
# this name avoids a conflict with ZIP in stepmake/aclocal.m4
STEPMAKE_PROGS(ZIPDOC, zip, $DOCUMENTATION_REQUIRED)
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
