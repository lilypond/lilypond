#!/bin/sh
# Run this to generate configure and initial GNUmakefiles

srcdir=`dirname $0`

case $1 in
    --noconf*) NOCONFIGURE=true;;
esac

if test -w "$srcdir"; then
  configure="$srcdir/configure"
  (
      cd "$srcdir"
      echo "Running autoconf ..."
      autoconf || exit 1
  )
else
  configure=./configure
  conf_flags="--srcdir $srcdir $conf_flags"
  echo "Running autoconf for read-only source directory ..."
  SRCDIR="$srcdir" autoconf -I "$srcdir" -o "$configure" "$srcdir/configure.ac" || exit 1
fi
# Autoconf automatically checks its own minimum required
# version, and it aborts when the check fails.
test "$?" -eq 1 && exit 1

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

echo Running $configure $conf_flags "$@" ...
"$configure" $conf_flags "$@"
