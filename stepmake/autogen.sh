#!/bin/sh
# Run this to generate configure and initial GNUmakefiles

srcdir=`dirname $0`
DIE=0

# autoconf > 2.50 is not very common yet,
# and disappointingly incompatible with the widely available 2.13
version=`autoconf --version 2>/dev/null | awk '{print $3}'`
if test "$version" != "2.13"; then
  echo "ERROR: Please install autoconf 2.13"
  exit 1
fi

if test -z "$*"; then
  echo "WARNING: I am going to run \`configure' with no arguments."
  echo "If you wish to pass any to it, please specify them on the"
  echo \`$0\'" command line."
  echo
fi

for coin in `find $srcdir -name configure.in -print`
do 
  dr=`dirname $coin`
  echo processing $dr
  (
      cd $dr
      echo "Running autoconf ..."
      autoconf
  )
done

#conf_flags="--enable-maintainer-mode --enable-compile-warnings" #--enable-iso-c
if test -z "$NOCONFIGURE"; then
  echo Running $srcdir/configure $conf_flags "$@" ...
  $srcdir/configure $conf_flags "$@"
else
  echo Skipping configure process.
fi
