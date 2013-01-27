#!/bin/sh
# Run this to generate configure and initial GNUmakefiles

srcdir=`dirname $0`

case $1 in
    --noconf*) NOCONFIGURE=true;;
esac

for i in $srcdir/configure.ac #`find $srcdir -name configure.ac -print`
do 
  dir=`dirname $i`
  echo processing $dir
  (
      cd $dir
      echo "Running autoconf ..."
      autoconf || exit 1
  )
  # Autoconf automatically checks its own minimum required
  # version, and it aborts when the check fails.
  test "$?" -eq 1 && exit 1
done

#conf_flags="--enable-maintainer-mode --enable-compile-warnings" #--enable-iso-c
if test -n "$NOCONFIGURE"; then
    echo Skipping configure process.
    exit 0
fi

if test -z "$*"; then
    cat <<EOF
    Warning: about to run \`configure' without arguments.
    arguments on the \`$0' command line
    will be passed to \`configure'.

    Invoke with --noconfigure to skip configure step.
EOF
fi

echo Running $srcdir/configure $conf_flags "$@" ...
$srcdir/configure $conf_flags "$@"
