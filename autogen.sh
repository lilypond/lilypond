#!/bin/sh
# WARNING WARNING WARNING
# do not edit! this is autogen.sh, generated from stepmake/autogen.sh
#!/bin/sh
# Run this to generate configure and initial GNUmakefiles

srcdir=`dirname $0`

# Be paranoid: check for autoconf == 2.13
# Some setups have both autoconf 2.13 and 2.50 available through
# a wrapper script: /usr/bin/autoconf.
# This wrapper may incorrectly autoselect autoconf 2.50, but it
# advertises itself as autoconf 2.13.
# If you have such a setup, invoke this script as:
#   autoconf=autoconf2.13 ./autogen.sh
for i in autoconf autoconf2.13 false; do
  version=`$i --version 2>/dev/null | head -1 | awk '{print $NF}' | awk -F. '{print $1 * 100 + $2}'`
  if test "0$version" -eq 213; then
    autoconf=$i
    break
  fi
done

if test -z "$autoconf"; then
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
      $autoconf
  )
done

#conf_flags="--enable-maintainer-mode --enable-compile-warnings" #--enable-iso-c
if test -z "$NOCONFIGURE"; then
  echo Running $srcdir/configure $conf_flags "$@" ...
  $srcdir/configure $conf_flags "$@"
else
  echo Skipping configure process.
fi
