dnl aclocal.m4   -*-shell-script-*-
dnl StepMake subroutines for configure.in

AC_DEFUN(AC_STEPMAKE_BIBTEX2HTML, [
    AC_CHECK_PROGS(BIBTEX2HTML, bibtex2html bib2html, error)
    if test "$BIBTEX2HTML" = "bib2html"; then
	BIBTEX2HTML_FLAGS='$< $(@)'
    else
	BIBTEX2HTML_FLAGS='-o $(@D)/$(*F) $<'
    fi
    AC_SUBST(BIBTEX2HTML)
    AC_SUBST(BIBTEX2HTML_FLAGS)
])


AC_DEFUN(AC_STEPMAKE_COMPILE, [
    # -O is necessary to get inlining
    CFLAGS=${CFLAGS:-""}
    CXXFLAGS=${CXXFLAGS:-$CFLAGS}
    LDFLAGS=${LDFLAGS:-""}
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
    LD='$(CC)'
    AC_SUBST(LD)

    CFLAGS="$CFLAGS $OPTIMIZE"
    CPPFLAGS=${CPPFLAGS:-""}

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

AC_DEFUN(AC_STEPMAKE_CXX, [
    AC_LANG_CPLUSPLUS
    AC_PROG_CXX

    AC_CHECK_HEADER(FlexLexer.h, true,
	AC_STEPMAKE_WARN(can"\'"t find flex header. Please install Flex headers correctly))

    CPPFLAGS="$CPPFLAGS $DEFINES"
    CXXFLAGS="$CXXFLAGS $OPTIMIZE"
    LDFLAGS="$LDFLAGS $EXTRA_LIBES"

    AC_SUBST(CXXFLAGS)
    AC_SUBST(CXX)
    LD='$(CXX)'
    AC_SUBST(LD)
])

AC_DEFUN(AC_STEPMAKE_CXXTEMPLATE, [
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

AC_DEFUN(AC_STEPMAKE_DATADIR, [
    if test "$datadir" = "\${prefix}/share"; then
	    datadir='${prefix}/share/'$package
    fi
    DIR_DATADIR=${datadir}
    presome=${prefix}
    if test "$prefix" = "NONE"; then
	    presome=${ac_default_prefix}
    fi
    DIR_DATADIR=`echo ${DIR_DATADIR} | sed "s!\\\${prefix}!$presome!"`

    AC_SUBST(datadir)
    AC_SUBST(DIR_DATADIR)
    
    dnl yeah, so fuck me gently with a cactus: this doesnt belong here
    dnl Please take the person responsible for inventing shell-scripts out
    dnl and shoot him. On behalf of the sane world, thank you.
    dnl DIR_SHAREDSTATEDIR="foobar"
    dnl AC_SUBST(DIR_SHAREDSTATEDIR)
    
    AC_DEFINE_UNQUOTED(DIR_DATADIR, "${DIR_DATADIR}")
])

AC_DEFUN(AC_STEPMAKE_END, [
    AC_OUTPUT($CONFIGFILE.make:config.make.in)

    # regular in-place build
    # test for srcdir_build = yes ?
    if test "$builddir" = "."; then
	rm -f $srcdir/GNUmakefile
	cp $srcdir/GNUmakefile.in $srcdir/GNUmakefile
	chmod 444 $srcdir/GNUmakefile
    else # --srcdir build
        rm -f GNUmakefile
    	cp $srcdir/make/srcdir.make.in GNUmakefile
    	chmod 444 GNUmakefile
    fi
])

AC_DEFUN(AC_STEPMAKE_GXX, [
    AC_MSG_CHECKING("g++ version")
    cxx_version=`$CXX --version`
    AC_MSG_RESULT("$cxx_version")
    changequote(<<, >>)dnl
    # urg, egcs: how to check for egcs >= 1.1?
    if expr "$cxx_version" : '.*2\.[89]' > /dev/null ||
	expr "$cxx_version" : '.*egcs' > /dev/null ||
	expr "$cxx_version" : '3\.0' > /dev/null
    changequote([, ])dnl
    then
	    true
    else
	    AC_STEPMAKE_WARN(can\'t find g++ 2.8, 2.9, 3.0 or egcs 1.1)
    fi
])

AC_DEFUN(AC_STEPMAKE_GUILE, [
    ## First, let's just see if we can find Guile at all.
    AC_MSG_CHECKING("for guile-config")
    for guile_config in guile-config $target-guile-config $build-guile-config; do
	AC_MSG_RESULT("$guile_config")
	if ! $guile_config --version > /dev/null 2>&1 ; then
	    AC_MSG_WARN("cannot execute $guile_config")
	    AC_MSG_CHECKING("if we are cross compiling")
	    guile_config=error
	else
	    break
	fi
    done
    if test "$guile_config" = "error"; then
	AC_MSG_ERROR("cannot find guile-config; is Guile installed?")
	exit 1
    fi
    AC_MSG_CHECKING("Guile version")
    need_guile_version="1.3.4"
    need_guile_version_numeric=100304
    guile_version=`$guile_config --version 2>&1 | awk '{print $NF}'`
    guile_version_numeric=`echo $guile_version | awk -F. '
{if ([$]3) {last = [$]3}
else {last =0}}
{printf "%s%s%s\n",[$]1*100, [$]2*10,last}'`
    AC_MSG_RESULT("$guile_version")
    if test $guile_version_numeric -lt $need_guile_version_numeric
    then
        AC_STEPMAKE_WARN("Guile version "$need_guile_version" or newer is needed")
    fi
    GUILE_FLAGS
    AC_PATH_PROG(GUILE, guile, error)
    AC_SUBST(GUILE)
])

AC_DEFUN(AC_STEPMAKE_INIT, [

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
    if test "x$PACKAGE" = "xSTEPMAKE"; then
	AC_MSG_RESULT(Stepmake package!)

	AC_MSG_CHECKING(builddir)
	if test "$srcdir" = "."; then
	    builddir=.
	else
	    absolute_builddir="`pwd`"
	    package_absolute_builddir="`dirname $absolute_builddir`"
	    package_srcdir="`dirname  $srcdir`"
	    builddir="`dirname $package_srcdir`/`basename $package_absolute_builddir`/`basename $absolute_builddir`"
	fi
	AC_MSG_RESULT($builddir)

	(cd stepmake 2>/dev/null || mkdir stepmake)
	(cd stepmake; rm -f stepmake; ln -s ../$srcdir/stepmake .)
	(cd stepmake; rm -f bin; ln -s ../$srcdir/bin .)
	AC_CONFIG_AUX_DIR(bin)
	stepmake=stepmake
    else
        AC_MSG_RESULT($PACKAGE)

	AC_MSG_CHECKING(builddir)
	if test "$srcdir" = "."; then
	    builddir=.
	    srcdir_build=no
	else
	    absolute_builddir="`pwd`"
#	    builddir="`dirname  $srcdir`/`basename $absolute_builddir`"
	    builddir="`bash $srcdir/buildscripts/walk.sh \"$srcdir\"`"
	    srcdir_build=yes
	fi
	AC_MSG_RESULT($builddir)
	if expr "$srcdir" : '/' > /dev/null 2>&1; then
	    absolute_srcdir=yes
	    AC_STEPMAKE_WARN(Absolute --srcdir specified: $srcdir)
	fi

	AC_MSG_CHECKING(for stepmake)
	# Check for installed stepmake
	if test -d $stepmake; then
	    AC_MSG_RESULT($stepmake)
	else
	    if test "$absolute_srcdir" != "yes"; then
		stepmake='$(depth)'/$srcdir/stepmake
	    else
		stepmake=$srcdir/stepmake
	    fi
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

    if test "$package_depth" = "" ; then
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

    test -n "$CONFIGURATION" && CONFIGSUFFIX="-$CONFIGURATION"
    CONFIGFILE=config$CONFIGSUFFIX
    AC_SUBST(CONFIGSUFFIX)
     
    AC_CANONICAL_HOST
    AC_CHECK_PROGS(MAKE, gmake make, error)
    AC_CHECK_PROGS(FIND, find, error)

dnl system supplied INSTALL is unsafe; use our own install.
dnl    AC_PROG_INSTALL
dnl    if test "$INSTALL" = "bin/install-sh"; then
dnl	export INSTALL="\$\(depth\)/bin/install-sh"
dnl    fi

    AC_CHECK_PROGS(TAR, tar, error)

    if test "x`uname`" = "xHP-UX"; then
	AC_PATH_PROG(BASH, bash, /bin/sh)
	AC_STEPMAKE_WARN(avoiding buggy /bin/sh)
	AC_PATH_PROG(SHELL, bash, /bin/ksh)
    else
	AC_PATH_PROG(BASH, bash, /bin/sh)
	SHELL=/bin/sh
	AC_SUBST(SHELL)
    fi


    AC_PATH_PROG(PYTHON, ${PYTHON:-python}, -echo no python)
    AC_SUBST(PYTHON)

    if test $MAKE != "error" ; then
	$MAKE -v 2> /dev/null | grep GNU > /dev/null
	if test "$?" = 1
	then
		AC_STEPMAKE_WARN(Please install *GNU* make) 
	fi
    fi 

    AC_CHECK_SEARCH_RESULT($PYTHON, python, You should install Python)

    if test "x$OSTYPE" = "xcygwin32" || test "x$OSTYPE" = "xWindows_NT"; then
	LN=cp # hard link does not work under cygnus-nt
	LN_S='cp -r' # symbolic link does not work for native nt
	ZIP="zip -r -9" #
	program_suffix=.exe
	# urg
	# ROOTSEP=':'
        # DIRSEP='\\'
 	# PATHSEP=';'
	#
	# cygwin fixes all these things.  
	# it seems these were used because of dos-style TEXINPUTS and
	# MFINPUTS needed for miktex.
	# but this breaks parsing of all other cygwin/unix style paths.
	#
	# if your (mik)tex breaks, make a:
	#    /usr/local/bin/tex:
	#    #!/bin/sh
	#    TEXINPUTS=`cygpath -pw $TEXINPUTS` /texmf/miktex/bin/tex $*
	#
	# and
	#
	#    /usr/local/bin/mf:
	#    #!/bin/sh
	#    MFINPUTS=`cygpath -pw $MFINPUTS` /texmf/miktex/bin/mf $*
	#
	# this way, you may have buildscripts/out/lilypond-profile 
	# 'automatically' sourced from /usr/etc/profile.d/ too.
	#
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
    AC_SUBST(PATHSEP)
    AC_SUBST(DIRSEP)
  
    AC_STEPMAKE_DATADIR
])

AC_DEFUN(AC_STEPMAKE_KPATHSEA, [

    kpathsea_b=yes
    AC_ARG_WITH(kpathsea,
    [  --with-kpathsea         use kpathsea lib.  Default: on],
    [kpathsea_b=$enableval])

    if test "$kpathsea_b" = "yes"; then	
	AC_HAVE_HEADERS(kpathsea/kpathsea.h)
	AC_CHECK_LIB(kpathsea, kpse_find_file)
	AC_CHECK_FUNCS(kpse_find_file,, AC_ERROR(Cannot find kpathsea functions.  You should install kpathsea; see INSTALL.txt.  Rerun ./configure --without-kpathsea only if kpathsea is not available for your platform.))
    fi
    AC_MSG_CHECKING(whether to use kpathsea)
    if test "$kpathsea_b" = yes; then
        AC_MSG_RESULT(yes)
	KPATHSEA=1
    else
        AC_MSG_RESULT(no)
	KPATHSEA=0
    fi

    AC_SUBST(KPATHSEA)
    AC_DEFINE_UNQUOTED(KPATHSEA, $KPATHSEA)
])

AC_DEFUN(AC_STEPMAKE_LEXYACC, [
    # ugh, automake: we want (and check for) bison
    AC_PROG_YACC
    # ugh, automake: we want (and check for) flex
    # AC_PROG_LEX
    # urg: automake 1.3: hope this doesn't break 1.2 ac_cv_pro_lex_root hack...

    # AC_DECL_YYTEXT
    # ugh, ugh
    ac_cv_prog_lex_root=lex.yy

    AC_CHECK_PROGS(BISON, bison, error)
    AC_CHECK_PROGS(FLEX, flex, error)
    AC_CHECK_SEARCH_RESULT($BISON, bison,  Please install Bison, 1.25 or newer)
    AC_CHECK_SEARCH_RESULT($FLEX,  flex, Please install Flex, 2.5 or newer)

    if test $BISON != "error"; then
	bison_version=`$BISON --version | sed 's/^.*version 1.//g'`
	if test `echo $bison_version | sed 's/\..*$//g'` -lt 25; then
	    AC_STEPMAKE_WARN(Your bison is a bit old (1.$bison_version). You might have to install 1.25)
	fi	
    fi

    AC_SUBST(BISON)
    AC_SUBST(FLEX)
])

AC_DEFUN(AC_STEPMAKE_LIB, [
    AC_CHECK_PROGS(AR, ar, error)
    AC_PROG_RANLIB

    AC_SUBST(AR)
    AC_SUBST(RANLIB)
])

AC_DEFUN(AC_STEPMAKE_LIBTOOL, [
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

AC_DEFUN(AC_STEPMAKE_LOCALE, [
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
	AC_STEPMAKE_WARN($language not supported; available are: $ALL_LINGUAS)
    fi

])

AC_DEFUN(AC_STEPMAKE_GETTEXT, [
    DIR_LOCALEDIR=${localedir}
    presome=${prefix}
    if test "$prefix" = "NONE"; then
	    presome=${ac_default_prefix}
    fi
    DIR_LOCALEDIR=`echo ${DIR_LOCALEDIR} | sed "s!\\\${prefix}!$presome!"`
    AC_SUBST(localedir)
    AC_SUBST(DIR_LOCALEDIR)
    AC_DEFINE_UNQUOTED(DIR_LOCALEDIR, "${DIR_LOCALEDIR}")

    AC_CHECK_LIB(intl, gettext)
    AC_CHECK_FUNCS(gettext)
])

AC_DEFUN(AC_STEPMAKE_MAKEINFO, [
    AC_CHECK_PROGS(MAKEINFO, makeinfo, error)
    if test "$MAKEINFO" != "error"; then
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
	    AC_STEPMAKE_WARN(your html documentation will be one large file)
	    rm -rf out/split
	fi
    fi
    AC_SUBST(SPLITTING_MAKEINFO)
])


AC_DEFUN(AC_STEPMAKE_MAN, [
    AC_CHECK_PROGS(GROFF, groff ditroff, -echo no groff)
    AC_CHECK_PROGS(TROFF, troff, -echo no troff)
    AC_CHECK_PROGS(TBL, tbl, cat)
])

AC_DEFUN(AC_STEPMAKE_MSGFMT, [
    # AC_CHECK_PROGS(MSGFMT, msgfmt, -echo no msgfmt)
    AC_CHECK_PROGS(MSGFMT, msgfmt, \$(SHELL) \$(step-bindir)/fake-msgfmt.sh )
    AC_MSG_CHECKING(whether msgfmt accepts -o)
    msgfmt_output="`msgfmt -o bla 2>&1 | grep usage`"
    if test "$msgfmt_output" = ""; then
	AC_MSG_RESULT(yes)
    else
	# urg
	MSGFMT="\$(SHELL) \$(step-bindir)/fake-msgfmt.sh"
	AC_MSG_RESULT(no)
	AC_STEPMAKE_WARN(please install msgfmt from GNU gettext)
    fi
    if test ! -n "$MSGFMT"; then
	AC_STEPMAKE_WARN(please install msgfmt from GNU gettext)
    fi
])

#why has this been dropped?
AC_DEFUN(XXAC_STEPMAKE_TEXMF_DIRS, [
    AC_ARG_ENABLE(tex-prefix,
    [  --enable-tex-prefix=DIR   set the tex-directory to find TeX
                               subdirectories.  Default: PREFIX],
    [TEXPREFIX=$enableval],
    [TEXPREFIX=auto] )
    
    AC_ARG_ENABLE(tex-dir,
    [  --enable-tex-dir=DIR      set the directory to put $PACKAGE_NAME TeX files in. ],
    [TEXDIR=$enableval],
    [TEXDIR=auto] )

    AC_ARG_ENABLE(mf-dir,
    [  --enable-mf-dir=DIR       set the directory to put $PACKAGE_NAME MetaFont files in. ],
    [MFDIR=$enableval],
    [MFDIR=auto])

    if test "x$TEXPREFIX" = xauto ; then
	AC_TEX_PREFIX(TEXPREFIX)
    else
     find_texprefix=$TEXPREFIX
    fi

    if test "x$MFDIR" = xauto; then
	AC_MF_SUBDIR(MFDIR)
    fi
	
    if test "x$TEXDIR" = xauto ; then
	AC_TEX_SUBDIR(TEXDIR)
    fi
    AC_SUBST(TEXPREFIX)
    AC_SUBST(TEXDIR)
    AC_SUBST(MFDIR)
])

AC_DEFUN(AC_STEPMAKE_TEXMF_DIRS, [
    AC_ARG_ENABLE(tfm-path,
    [  --enable-tfm-path=PATH  set path of tex directories where tfm files live,
                            esp.: cmr10.tfm.  Default: use kpsewhich],
    [tfm_path=$enableval],
    [tfm_path=auto] )

    AC_CHECK_PROGS(KPSEWHICH, kpsewhich, no)
    AC_MSG_CHECKING(for tfm path)

    TFM_FONTS="cmr msam"

    if test "x$tfm_path" = xauto ; then
	if test "x$KPSEWHICH" != "xno" ; then
	    for i in $TFM_FONTS; do
		dir=`$KPSEWHICH tfm ${i}10.tfm`
		TFM_PATH="$TFM_PATH `dirname $dir`"
	    done
	else
	    AC_STEPMAKE_WARN(Please specify where cmr10.tfm lives:
    ./configure --enable-tfm-path=/usr/local/TeX/lib/tex/fonts)
	fi
    else
         TFM_PATH=$tfm_path
    fi

    TFM_PATH=`echo $TFM_PATH | tr ':' ' '`
    AC_MSG_RESULT($TFM_PATH)
    AC_SUBST(TFM_PATH)
])

AC_DEFUN(AC_STEPMAKE_TEXMF, [
    # urg, never know what names these teTeX guys will think up

    AC_CHECK_PROGS(METAFONT, mf, no)
    if test "x$METAFONT" = "xno"; then
	AC_CHECK_PROGS(MFONT, mfont, -echo no mf or mfont)
	METAFONT=$MFONT
    fi

    AC_CHECK_PROGS(METAPOST, mp, no)
    if test "x$METAPOST" = "xno"; then
	AC_CHECK_PROGS(MPOST, mpost, -echo no mp or mpost)

	METAPOST=$MPOST
    fi

    AC_CHECK_PROGS(INIMETAFONT, inimf, no)
    if test "x$INIMETAFONT" = "xno"; then
	AC_CHECK_PROGS(INIMFONT, inimfont, -echo no inimf or inimfont)
	INIMETAFONT=$INIMFONT
    fi

    AC_CHECK_PROGS(INIMETAPOST, inimp, no)
    if test "x$INIMETAPOST" = "xno"; then
	AC_CHECK_PROGS(INIMPOST, inimpost, -echo no inimp or inimpost)
	INIMETAPOST=$INIMPOST
    fi

    AC_MSG_CHECKING(for working metafont mode)
    modelist='ljfour lj4 lj3 lj2 ljet laserjet'
    for MFMODE in $modelist; do
    	$METAFONT "\mode:=$MFMODE; mode_setup; end." > /dev/null 2>&1
	if test -f mfput.tfm; then
	    break;
	fi
    done
    AC_MSG_RESULT($MFMODE)

    AC_MSG_CHECKING(for mfplain.mp)
    #
    # For now let people define these in their environments
    #
    : ${MFPLAIN_MP=`kpsewhich mp mfplain.mp`}
    AC_MSG_RESULT($MFPLAIN_MP)

    AC_MSG_CHECKING(for inimetapost flags)
    if test  ${INIMETAPOST} = "inimp" ; then
       : ${INIMETAPOST_FLAGS=''}
    else
       : ${INIMETAPOST_FLAGS='-interaction=nonstopmode'}
    fi
    AC_MSG_RESULT($INIMETAPOST_FLAGS)

    rm -f mfput.*

    AC_SUBST(METAFONT)
    AC_SUBST(METAPOST)
    AC_SUBST(MFMODE)
    AC_SUBST(INIMETAFONT)
    AC_SUBST(INIMETAPOST)
    AC_SUBST(MFPLAIN_MP)
    AC_SUBST(INIMETAPOST_FLAGS)
])

AC_DEFUN(AC_STEPMAKE_WARN, [
    AC_MSG_WARN($1)
    warn_b=yes
])

AC_DEFUN(AC_STEPMAKE_YODL, [
    if test "x$YODL" = "x"; then 
	AC_CHECK_PROGS(STRIPROFF, striproff, -echo no striproff)
	AC_CHECK_PROGS(YODL, yodl, -echo no yodl)
	AC_CHECK_PROGS(YODL2HTML, yodl2html, -echo no yodl)
	AC_CHECK_PROGS(YODL2LATEX, yodl2latex, )
	AC_CHECK_PROGS(YODL2MAN, yodl2man, -echo no yodl)
	AC_CHECK_PROGS(YODL2MSLESS, yodl2msless, -echo no yodl)
	AC_CHECK_PROGS(YODL2TEXINFO, yodl2texinfo, -echo no yodl)
	AC_CHECK_PROGS(YODL2TXT, yodl2txt, -echo no yodl)
	YODL2LESS_DIR='$(bindir)/'
    else
	AC_SUBST(STRIPROFF)
	AC_SUBST(YODL)
	AC_SUBST(YODL2HTML)
	AC_SUBST(YODL2LATEX)
	AC_SUBST(YODL2LESS_DIR)
	AC_SUBST(YODL2MAN)
	AC_SUBST(YODL2MSLESS)
	AC_SUBST(YODL2TEXINFO)
	AC_SUBST(YODL2TXT)
	export STRIPROFF YODL YODL2HTML YODL2LATEX YODL2MAN YODL2MSLESS YODL2TEXINFO YODL2TXT
    fi
    if test "x$YODL" = "-echo no yodl"; then
	AC_STEPMAKE_WARN(Did not find YODL (Yodl is Yet Oneother Document Language, see http://www.cs.uu.nl/~hanwen/yodl))
    fi    
])

dnl should cache result.
dnl should  look in $prefix first.
dnl should probably assume TDS

AC_DEFUN(AC_TEX_PREFIX, [
    

    AC_MSG_CHECKING(TeX/MF root dir directory)    

    find_root_prefix="$prefix"
    

    test "x$find_root_prefix" = xNONE && find_root_prefix="$ac_default_prefix"
    find_texpostfix="";
    for postfix in "/lib/tex/" "/lib/texmf" "/lib" "/tex" "/texmf"; do
	find_texprefix="$find_root_prefix$postfix"
	if test -d $find_texprefix; then
	    find_texpostfix=$postfix
	    break;
	fi
    done
    
    if test "x$find_texpostfix" = x; then
	find_texpostfix='/lib/texmf/tex'
	AC_STEPMAKE_WARN(Cannot determine the TeX-directory.  Please use --enable-tex-prefix)
    fi

    find_texprefix="$find_root_prefix/$find_texpostfix"

    # only assign if variablename not empty
    if test x != "x[$]$1"; then
    	$1='${prefix}'/"$find_texpostfix"
    fi
    AC_MSG_RESULT($find_texprefix)

])
 

# find a directory inside a prefix, 
# $1 the prefix (expanded version)
# $2 variable to assign
# $3 the directory name 
# $4 description
AC_DEFUN(AC_FIND_DIR_IN_PREFIX, [
    
    AC_MSG_CHECKING($4 directory)    
    find_dirdir=`(cd $1; 
      $FIND ./ -type d -a -name $3 -print |sort|head -1|sed 's#^\./##')`
    

    if test "x$find_dirdir" = x; then
       find_dirdir="/$3";
       AC_STEPMAKE_WARN(Cannot determine $4 subdirectory.  Please set from command-line)
	true
    fi
    $2=$find_dirdir
    AC_MSG_RESULT($1/$find_dirdir)
])

# ugh.  this is hopeless
AC_DEFUN(AC_KPSE_TEX_DIR, [
	kpse_paths=`(kpsepath -n latex tex; kpsepath -n tex tex) | sed 's/:/ /g' | tr ' ' '\012' |sort | uniq -d`
	kpse_syspaths=`echo $kpse_paths | grep '!'| sed 's/!//g'`
	echo $kpse_paths
	if test -w "$kpse_syspaths";
	then
		dir=`echo $kpse_syspaths | head -1`
	else
		dir=`echo $kpse_paths | grep -v '!'| head -1`
	fi
	if test "$prefix" = "NONE"; then
		local_prefix=$ac_default_prefix
		local_prefix_quote='${prefix}'

	else
		local_prefix=$prefix
		local_prefix_quote=$prefix
	fi
	echo $local_prefix_quote = $local_prefix
	echo $dir
	echo $dir  | sed 's!'$local_prefix'!\$local_prefix_quote!g'
])

AC_DEFUN(AC_TEX_SUBDIR, [
dnl    AC_REQUIRE([AC_TEX_PREFIX])
    AC_FIND_DIR_IN_PREFIX($find_texprefix, $1, tex,TeX input)
    $1="$TEXPREFIX/$$1"
])

AC_DEFUN(AC_MF_SUBDIR, [
dnl     AC_REQUIRE([AC_TEX_PREFIX])
    AC_FIND_DIR_IN_PREFIX($find_texprefix, $1, source, MF input)
    $1="$TEXPREFIX/$$1"
])

AC_DEFUN(AC_CHECK_SEARCH_RESULT, [
	result="`echo \"$1\" | grep echo`"
	if test "x$1" = "xerror" -o "x$result" != "x"; then
		AC_STEPMAKE_WARN(can\'t find $2. $3)
	fi
])

dnl   GUILE_FLAGS --- set flags for compiling and linking with Guile
dnl
dnl   This macro runs the `guile-config' script, installed with Guile,
dnl   to find out where Guile's header files and libraries are
dnl   installed.  It sets two variables, marked for substitution, as
dnl   by AC_SUBST.
dnl   
dnl     GUILE_CFLAGS --- flags to pass to a C or C++ compiler to build
dnl             code that uses Guile header files.  This is almost
dnl             always just a -I flag.
dnl   
dnl     GUILE_LDFLAGS --- flags to pass to the linker to link a
dnl             program against Guile.  This includes `-lguile' for
dnl             the Guile library itself, any libraries that Guile
dnl             itself requires (like -lqthreads), and so on.  It may
dnl             also include a -L flag to tell the compiler where to
dnl             find the libraries.

AC_DEFUN([GUILE_FLAGS],[
## The GUILE_FLAGS macro.
  AC_MSG_CHECKING(for Guile)
  if ! $guile_config link > /dev/null ; then
      AC_MSG_RESULT("cannot execute $guile_config")
      AC_MSG_ERROR("cannot find guile-config; is Guile installed?")
      exit 1
  fi
  GUILE_CFLAGS="`$guile_config compile`"
  GUILE_LDFLAGS="`$guile_config link`"
  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
  AC_MSG_RESULT(yes)
])


# Configure paths for GTK+
# Owen Taylor     97-11-3

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTK,
[dnl 
dnl Get the cflags and libraries from the gtk-config script
dnl
  AC_PATH_PROG(GTK_CONFIG, gtk-config, no)
  min_gtk_version=ifelse([$1], ,1.1.1,$1)
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  if test "$GTK_CONFIG" != "no" ; then
    GTK_CFLAGS=`$GTK_CONFIG --cflags`
    GTK_LIBS=`$GTK_CONFIG --libs`
    ac_save_CFLAGS="$CFLAGS"
    ac_save_LIBS="$LIBS"
    ac_save_CXXFLAGS="$CXXFLAGS"
    CFLAGS="$CFLAGS $GTK_CFLAGS"
    CXXFLAGS="$CXXFLAGS $GTK_CFLAGS"
    LIBS="$LIBS $GTK_LIBS"
dnl
dnl Now check if the installed GTK is sufficiently new. (Also sanity
dnl checks the results of gtk-config to some extent)
dnl
    AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>

int 
main ()
{
  int major, minor, micro;

  if (sscanf("$min_gtk_version", "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

   return !((gtk_major_version > major) ||
   	    ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
 	    ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)));
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
     CFLAGS="$ac_save_CFLAGS"
     CXXFLAGS="$ac_save_CXXFLAGS"
     LIBS="$ac_save_LIBS"
  else
     no_gtk=yes
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  CXXFLAGS="$CXXFLAGS $GTK_CFLAGS"
  AC_SUBST(CXXFLAGS)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
])


# Configure paths for GTK--
# Erik Andersen	30 May 1998
# Modified by Tero Pulkkinen (added the compiler checks... I hope they work..)

dnl Test for GTK__, and define GTK___CFLAGS and GTK___LIBS
dnl   to be used as follows:
dnl AM_PATH_GTKMM([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl

dnl Get the cflags and libraries from the gtkmm-config script
dnl
AC_ARG_WITH(gtkmm-prefix,[  --with-gtkmm-prefix=PREFIX
                          Prefix where GTK-- is installed (optional)],
            gtkmm_config_prefix="$withval", gtkmm_config_prefix="")
AC_ARG_WITH(gtkmm-exec-prefix,[  --with-gtkmm-exec-prefix=PREFIX
                          Exec prefix where GTK-- is installed (optional)],
            gtkmm_config_exec_prefix="$withval", gtkmm_config_exec_prefix="")
AC_ARG_ENABLE(gtkmmtest, [  --disable-gtkmmtest     Do not try to compile and run a test GTK-- program],
		    , enable_gtkmmtest=yes)

  if test x$gtkmm_config_exec_prefix != x ; then
     gtkmm_config_args="$gtkmm_config_args --exec-prefix=$gtkmm_config_exec_prefix"
     if test x${GTKMM_CONFIG+set} != xset ; then
        GTKMM_CONFIG=$gtkmm_config_exec_prefix/bin/gtkmm-config
     fi
  fi
  if test x$gtkmm_config_prefix != x ; then
     gtkmm_config_args="$gtkmm_config_args --prefix=$gtkmm_config_prefix"
     if test x${GTKMM_CONFIG+set} != xset ; then
        GTKMM_CONFIG=$gtkmm_config_prefix/bin/gtkmm-config
     fi
  fi


AC_DEFUN(AM_PATH_GTKMM,
[dnl 

dnl
dnl Check if the installed GTK-- is sufficiently new.
dnl
  AC_PATH_PROG(GTKMM_CONFIG, gtkmm-config, no)
  min_gtkmm_version=ifelse([$1], ,0.9.14,$1)

  AC_MSG_CHECKING(for GTK-- - version >= $min_gtkmm_version)
  no_gtkmm=""
  if test "$GTKMM_CONFIG" = "no" ; then
    no_gtkmm=yes
  else
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS

    GTK___CFLAGS=`$GTKMM_CONFIG $gtkmm_config_args --cflags`
    GTK___LIBS=`$GTKMM_CONFIG $gtkmm_config_args --libs`
    gtkmm_config_major_version=`$GTKMM_CONFIG $gtkmm_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtkmm_config_minor_version=`$GTKMM_CONFIG $gtkmm_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtkmm_config_micro_version=`$GTKMM_CONFIG $gtkmm_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gtkmmtest" = "xyes" ; then
      ac_save_CXXFLAGS="$CXXFLAGS"
      ac_save_LIBS="$LIBS"
      CXXFLAGS="$CXXFLAGS $GTK___CFLAGS"
      LIBS="$LIBS $GTK___LIBS"
dnl
dnl Now check if the installed GTK-- is sufficiently new. (Also sanity
dnl checks the results of gtkmm-config to some extent
dnl
      rm -f conf.gtkmmtest
      AC_TRY_RUN([
#include <gtk--.h>
#include <stdio.h>
#include <stdlib.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtkmmtest");

  /* HP/UX 0 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtkmm_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtkmm_version");
     exit(1);
   }

  if ((gtkmm_major_version != $gtkmm_config_major_version) ||
      (gtkmm_minor_version != $gtkmm_config_minor_version) ||
      (gtkmm_micro_version != $gtkmm_config_micro_version))
    {
      printf("\n*** 'gtkmm-config --version' returned %d.%d.%d, but GTK-- (%d.%d.%d)\n", 
             $gtkmm_config_major_version, $gtkmm_config_minor_version, $gtkmm_config_micro_version,
             gtkmm_major_version, gtkmm_minor_version, gtkmm_micro_version);
      printf ("*** was found! If gtkmm-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK--. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
      printf("*** If gtkmm-config was wrong, set the environment variable GTKMM_CONFIG\n");
      printf("*** to point to the correct copy of gtkmm-config, and remove the file config.cache\n");
      printf("*** before re-running configure\n");
    } 
/* GTK-- does not have the GTKMM_*_VERSION constants */
/* 
  else if ((gtkmm_major_version != GTKMM_MAJOR_VERSION) ||
	   (gtkmm_minor_version != GTKMM_MINOR_VERSION) ||
           (gtkmm_micro_version != GTKMM_MICRO_VERSION))
    {
      printf("*** GTK-- header files (version %d.%d.%d) do not match\n",
	     GTKMM_MAJOR_VERSION, GTKMM_MINOR_VERSION, GTKMM_MICRO_VERSION);
      printf("*** library (version %d.%d.%d)\n",
	     gtkmm_major_version, gtkmm_minor_version, gtkmm_micro_version);
    }
*/
  else
    {
      if ((gtkmm_major_version > major) ||
        ((gtkmm_major_version == major) && (gtkmm_minor_version > minor)) ||
        ((gtkmm_major_version == major) && (gtkmm_minor_version == minor) && (gtkmm_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK-- (%d.%d.%d) was found.\n",
               gtkmm_major_version, gtkmm_minor_version, gtkmm_micro_version);
        printf("*** You need a version of GTK-- newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK-- is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the gtkmm-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GTK--, but you can also set the GTKMM_CONFIG environment to point to the\n");
        printf("*** correct copy of gtkmm-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
      }
    }
  return 1;
}
],, no_gtkmm=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CXXFLAGS="$ac_save_CXXFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gtkmm" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GTKMM_CONFIG" = "no" ; then
       echo "*** The gtkmm-config script installed by GTK-- could not be found"
       echo "*** If GTK-- was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GTK_CONFIG environment variable to the"
       echo "*** full path to gtk-config."
       echo "*** The gtkmm-config script was not available in GTK-- versions"
       echo "*** prior to 0.9.12. Perhaps you need to update your installed"
       echo "*** version to 0.9.12 or newer"
     else
       if test -f conf.gtkmmtest ; then
        :
       else
          echo "*** Could not run GTK-- test program, checking why..."
          CXXFLAGS="$CFLAGS $GTKMM_CXXFLAGS"
          LIBS="$LIBS $GTK___LIBS"
          AC_TRY_LINK([
#include <gtk--.h>
#include <stdio.h>
],      [ return ((gtkmm_major_version) || (gtkmm_minor_version) || (gtkmm_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK-- or finding the wrong"
          echo "*** version of GTK--. If it is not finding GTK--, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK-- was incorrectly installed"
          echo "*** or that you have moved GTK-- since it was installed. In the latter case, you"
          echo "*** may want to edit the gtkmm-config script: $GTKMM_CONFIG" ])
          CXXFLAGS="$ac_save_CXXFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK___CFLAGS=""
     GTK__LIBS=""
     ifelse([$3], , :, [$3])
     AC_LANG_RESTORE
  fi
  AC_SUBST(GTK___CFLAGS)
  AC_SUBST(GTK___LIBS)
  rm -f conf.gtkmmtest
])

# Configure paths for GTK--DRAW
# Derek Quinn Wyatt   98-08-21  (adapted from Jan Nieuwenhuizen's code)

dnl AM_PATH_GTK__DRAW([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GTK--DRAW, and define GTK___CFLAGS and GTK___LIBS
dnl
AC_DEFUN(AM_PATH_GTK__DRAW,
[dnl 
dnl Get the cflags and libraries from the gtk__-config script
dnl
  AC_PATH_PROG(GTKMM_CONFIG, gtkmm-config, no)
  min_gtk___version=ifelse([$1], ,0.0.5,$1)
  AC_MSG_CHECKING(for GTK--DRAW - version >= $min_gtk___version)
  no_gtk__=""
  if test "$GTKMM_CONFIG" != "no" ; then
    GTK___CFLAGS=`$GTKMM_CONFIG --cflags`
    GTK___LIBS=`$GTKMM_CONFIG --libs`
    GTK___DLIBS="$GTK___LIBS -lgtkmmdraw"
    GTK___LIBS="$GTK___DLIBS"
    ac_save_CFLAGS="$CFLAGS"
    ac_save_LIBS="$LIBS"
    ac_save_CXXFLAGS="$CXXFLAGS"
    CFLAGS="$CFLAGS $GTK___CFLAGS"
    CXXFLAGS="$CXXFLAGS $GTK___CFLAGS"
    LIBS="$LIBS $GTK___LIBS"
dnl
dnl Now check if the installed GTK__ is sufficiently new. (Also sanity
dnl checks the results of gtk__-config to some extent)
dnl
    AC_TRY_RUN([
#include <gtk--.h>
#include <stdio.h>

int 
main ()
{
  // urg
  return 0;
}
],, no_gtk__=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
     CFLAGS="$ac_save_CFLAGS"
     CXXFLAGS="$ac_save_CXXFLAGS"
     LIBS="$ac_save_LIBS"
  else
     no_gtk__=yes
  fi
  if test "x$no_gtk__" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     GTK___CFLAGS=""
     GTK___LIBS=""
     ifelse([$3], , :, [$3])
  fi
  CXXFLAGS="$CXXFLAGS $GTK___CFLAGS"
  AC_SUBST(CXXFLAGS)
  AC_SUBST(GTK___CFLAGS)
  AC_SUBST(GTK___LIBS)
])
