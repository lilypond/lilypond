#!/bin/sh
# Run this to generate configure and initial GNUmakefiles

srcdir=`dirname $0`

case $1 in
    --noconf*) NOCONFIGURE=true;;
esac

touch stepmake/bin/install-sh
if [ ! -f aclocal.m4 -o stepmake/aclocal.m4 -nt aclocal.m4 ]; then
    echo "stepmake/aclocal.m4 is newer.  Copying file." 
    cp -f stepmake/aclocal.m4 aclocal.m4
fi

if [ ! -f autogen.sh -o stepmake/autogen.sh -nt autogen.sh ]; then
    echo "stepmake/autogen.sh is newer.  Copying file." 
    cp -f stepmake/autogen.sh autogen.sh
    exec ./autogen.sh "$@"
fi

# Be paranoid: check for autoconf >= 2.50
# Some setups have both autoconf 2.13 and 2.5x available through
# a wrapper script: /usr/bin/autoconf.
# This wrapper may incorrectly autoselect autoconf 2.50, but it
# advertises itself as autoconf 2.13.
# If you have such a setup, invoke this script as:
#   autoconf=autoconf2.50 ./autogen.sh
for i in autoconf2.50 autoconf-2.50 autoconf  false; do
  version=`$i --version 2>/dev/null | head -n 1 | awk '{print $NF}' | awk -F. '{print $1 * 100 + $2}'`
  if test "0$version" -ge 250; then
    autoconf=$i
    break
  fi
done

if test -z "$autoconf"; then
    echo "ERROR: Please install autoconf 2.50 or newer"
    exit 1
fi

for i in $srcdir/configure.in #`find $srcdir -name configure.in -print`
do 
  dir=`dirname $i`
  echo processing $dir
  (
      cd $dir
      echo "Running autoconf ..."
      $autoconf
  )
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
