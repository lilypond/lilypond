#!/bin/sh
# WARNING WARNING WARNING
# do not edit! this is autogen.sh, generated from stepmake/autogen.sh
#!/bin/sh
# Run this to generate configure and initial GNUmakefiles

srcdir=`dirname $0`
DIE=0

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "ERROR: You must have \`autoconf' installed to."
  echo "Download the appropriate package for your distribution,"
  echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
}

if test "$DIE" -eq 1; then
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
