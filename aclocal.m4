dnl aclocal.m4 generated automatically by aclocal 1.2


AC_DEFUN(AC_JUNK_ARGS, [
])

AC_DEFUN(AC_LILY_WARN, [
    AC_MSG_WARN($1)
    warn_b=yes
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
	AC_LILY_WARN(Cannot determine the TeX-directory. Please use --enable-tex-prefix)
    fi

    find_texprefix="$find_root_prefix/$find_texpostfix"

    # only assign if variablename not empty
    if test x != "x$1"; then
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
       AC_LILY_WARN(Cannot determine $4 subdirectory. Please set from command-line)
	true
    fi
    $2=$find_dirdir
    AC_MSG_RESULT($1/$find_dirdir)
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
	if test $1 = "error" 
	then
		AC_LILY_WARN(can't find $2. $3)
	fi
])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AM_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
AC_DEFINE_UNQUOTED(VERSION, "$VERSION"))
AM_SANITY_CHECK
AC_ARG_PROGRAM
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_PROG_MAKE_SET])


# serial 1

AC_DEFUN(AM_PROG_INSTALL,
[AC_REQUIRE([AC_PROG_INSTALL])
test -z "$INSTALL_SCRIPT" && INSTALL_SCRIPT='${INSTALL_PROGRAM}'
AC_SUBST(INSTALL_SCRIPT)dnl
])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "$@" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])

