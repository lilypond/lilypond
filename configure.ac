dnl configure.ac   -*-shell-script-*-
dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.60)

# Bootstrap the init process.
AC_INIT

# Bootstrap StepMake configure
AC_CONFIG_AUX_DIR([config])

STEPMAKE_INIT

# List a file that identifies your package.
AC_CONFIG_SRCDIR([lily/main.cc])

# Move to aclocal.m4?
AC_CONFIG_HEADERS([$CONFIGFILE.hh:config.hh.in])

# Uncomment the configuration options your package needs.

DOCUMENTATION=yes
AC_ARG_ENABLE(documentation,
    [AS_HELP_STRING([--enable-documentation],
		    [build Documentation.  Default: on])],
    [DOCUMENTATION=$enableval])
AC_SUBST(DOCUMENTATION)

GUILEv2=no
AC_ARG_ENABLE(guile2,
    [AS_HELP_STRING([--enable-guile2],
		    [highly experimental GUILE 2 support.  Default: off])],
    [GUILEv2=$enableval])

AC_ARG_WITH(texgyre-dir,
    [AS_HELP_STRING([--with-texgyre-dir=DIR],
		    [location of TeX Gyre fonts.])],
    [TEXGYRE_DIR=$withval],
    [TEXGYRE_DIR=""])

reloc_b=no
AC_ARG_ENABLE(relocation,
    [AS_HELP_STRING([--enable-relocation],
		    [compile with dynamic relocation.  Default: off])],
    [reloc_b=$enableval])

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

AC_LANG([C++])

STEPMAKE_PYTHON(REQUIRED, 2.4, 3.0)
# this checks if we have GNU C by compiling a program with
# __GNUC__, but that macro now unofficially means "the compiler
# supports the GNU C extensions" -- the intel C compiler and clang
# both support __GNUC__!
STEPMAKE_GCC_OR_CLANG(REQUIRED, 3.4)

STEPMAKE_CXX(REQUIRED)
STEPMAKE_GXX_OR_CLANG(REQUIRED, 3.4)
STEPMAKE_GXXCODEGENBUG
STEPMAKE_CXXTEMPLATE
STEPMAKE_STL_DATA_METHOD
STEPMAKE_LIB(REQUIRED)
STEPMAKE_DLOPEN
# Do not use bison 1.50 and 1.75.
# 1.29 is required fr %locations, but I'm not sure it's enough --ns
STEPMAKE_BISON(REQUIRED, 1.29)
STEPMAKE_FLEX(REQUIRED)
STEPMAKE_FLEXLEXER(REQUIRED)
STEPMAKE_FLEXLEXER_LOCATION
STEPMAKE_LOCALE
STEPMAKE_GETTEXT
STEPMAKE_MSGFMT(REQUIRED)
STEPMAKE_TEXMF(REQUIRED)
STEPMAKE_TEXMF_DIRS(REQUIRED)
if test "$GUILEv2" = "yes"
then
    STEPMAKE_GUILE_DEVEL(REQUIRED, 2.0.7, 2.2.0)
else
    STEPMAKE_GUILE_DEVEL(REQUIRED, 1.8.2, 1.9.0)
fi

# check for 3 typedefs added in Guile 1.9
save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$GUILE_CFLAGS $CXXFLAGS"
AC_CHECK_TYPES([scm_t_hash_fold_fn, scm_t_hash_handle_fn],
	       [AC_DEFINE(HAVE_GUILE_HASH_FUNC)], [],
	       [#include <libguile.h>])
AC_CHECK_TYPES([scm_t_subr],
	       [AC_DEFINE(HAVE_GUILE_SUBR_TYPE)], [],
	       [#include <libguile.h>])
CXXFLAGS="$save_CXXFLAGS"

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

## check rational bugfix.
save_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$GUILE_CFLAGS $CPPFLAGS"
AC_MSG_CHECKING(GUILE rational bugfix)
AC_PREPROC_IFELSE([AC_LANG_SOURCE([[#include <libguile.h>
#ifdef SCM_FRACTION_REDUCED_BIT
#error
#endif
]])],[AC_MSG_RESULT(ok)],[REQUIRED="$REQUIRED GUILE-with-rational-bugfix"
AC_MSG_RESULT(Must have patched GUILE rational support. See INSTALL.txt)])
CPPFLAGS="$save_CPPFLAGS"


STEPMAKE_PYTHON_DEVEL(REQUIRED, 2.4, 3.0)

STEPMAKE_PATH_PROG(FONTFORGE, fontforge, REQUIRED, 20110222)

STEPMAKE_PATH_PROG(T1ASM, t1asm, REQUIRED)

AC_CHECK_HEADERS([assert.h grp.h libio.h pwd.h sys/stat.h wchar.h fpu_control.h])
AC_CHECK_HEADERS([sstream])
AC_HEADER_STAT
AC_FUNC_MEMCMP
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([chroot fopencookie gettext isinf memmem snprintf vsnprintf])

STEPMAKE_PROGS(PKG_CONFIG, pkg-config, REQUIRED, 0.9.0)

AC_MSG_CHECKING(whether to enable dynamic relocation)
if test "$reloc_b" = "yes"; then
    AC_DEFINE(ARGV0_RELOCATION)
fi
AC_MSG_RESULT($reloc_b)

AC_MSG_CHECKING(for rpath linkage)
if test "$rpath_b" = "yes"; then
    LDFLAGS="$LDFLAGS -Wl,-rpath -Wl,\\\$\$ORIGIN/../lib"
elif test "$rpath_b" != "no"; then
    LDFLAGS="$LDFLAGS -Wl,-rpath -Wl,$rpath_b"
fi
AC_MSG_RESULT($rpath_b)

HOST_ARCH=`$CC -dumpmachine`
AC_SUBST(HOST_ARCH)

STEPMAKE_PANGO_FT2(pangoft2, REQUIRED, 1.6.0)
STEPMAKE_FONTCONFIG(fontconfig, REQUIRED, 2.4.0)
STEPMAKE_FREETYPE2(freetype2, REQUIRED, 2.1.10)

STEPMAKE_WINDOWS

# guile executable for some scripts
STEPMAKE_GUILE(OPTIONAL, 1.8.2, 1.9.0)

# perl for help2man and for mf2pt1.pl
STEPMAKE_PERL(REQUIRED)

## Optional tools for building documentation, website, extra fonts.
if test "$DOCUMENTATION" = "yes"; then
  DOCUMENTATION_REQUIRED=REQUIRED
else
  DOCUMENTATION_REQUIRED=OPTIONAL
fi

STEPMAKE_PATH_PROG(GHOSTSCRIPT, gs, $DOCUMENTATION_REQUIRED, 8.60)

STEPMAKE_PROGS(MAKEINFO, makeinfo, REQUIRED, 4.11)
STEPMAKE_PROGS(TEXI2HTML, texi2html, $DOCUMENTATION_REQUIRED, 1.82)
STEPMAKE_PROGS(DBLATEX, dblatex, $DOCUMENTATION_REQUIRED, 0.1.4)
STEPMAKE_PROGS(BIBTEX, bibtex, $DOCUMENTATION_REQUIRED)
STEPMAKE_PROGS(PDFLATEX, xelatex pdflatex, $DOCUMENTATION_REQUIRED)
if test "$PDFLATEX" = "xelatex"; then
    STEPMAKE_PROGS(PDFTEX, xetex, $DOCUMENTATION_REQUIRED)
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
STEPMAKE_PROGS(NETPBM, pngtopnm, $DOCUMENTATION_REQUIRED)
STEPMAKE_PROGS(IMAGEMAGICK, convert, $DOCUMENTATION_REQUIRED)
# this name avoids a conflict with ZIP in stepmake/aclocal.m4
STEPMAKE_PROGS(ZIPDOC, zip, $DOCUMENTATION_REQUIRED)
STEPMAKE_PROGS(RSYNC, rsync, $DOCUMENTATION_REQUIRED)

AC_DEFINE_UNQUOTED(FLOWER_VERSION, "${FULL_FLOWER_VERSION}")

# must also have local.make if configure fails.
touch local.make

if test "$reloc_b$rpath_b" = "yesno"; then
    STEPMAKE_WARN([Using --enable-relocation without --enable-rpath.  LilyPond may have trouble finding libraries.])
fi

# Gather requirements and generate output.
STEPMAKE_END

test -n "$CONFIGURATION" && mc=" conf=$CONFIGURATION" || mc=""

if make top-doc 1>/dev/null 2>&1; then
    rm -f INSTALL.txt && ln Documentation/topdocs/out/INSTALL.txt .
    rm -f README.txt && ln Documentation/topdocs/out/README.txt .
fi

cat <<EOF

Type:
    make$mc all       to build LilyPond
    make$mc install   to install LilyPond
    make$mc help      to see all possible targets

Edit local.make for local Makefile overrides.


EOF
