
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
