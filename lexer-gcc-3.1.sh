#!/bin/sh
#
# script documenting fixes for flex-2.5.4 and gcc-3.1

set -e

includes="$HOME/usr/include /usr/local/include /usr/include"

for i in $includes; do
    file=$i/FlexLexer.h
    if [ -f "$file" ]; then
	break
    else
	file=
    fi
done

if [ -z "$file" ]; then
    cat <<EOF
FlexLexer.h not found in $includes

Please install flex, or find and fix FlexLexer.h by hand.
EOF
    exit 1
fi


if [ -n "$CONF" ]; then
    CONFIGSUFFIX=-$CONF
    SETCONF="CONF=$CONF "
    setconf="conf=$CONF "
    ENABLECONFIG="--enable-config=$CONF "
fi    
outdir=out$CONFIGSUFFIX

echo -n "Copying and fixing $file... "
mkdir -p lily/$outdir
rm -f lily/$outdir/FlexLexer.h
sed -e 's/iostream.h/iostream/' \
    -e 's/\<istream\>/std::istream/' \
    -e 's/\<ostream\>/std::ostream/' \
    $file > lily/$outdir/FlexLexer.h
echo "done"


if [ -f GNUmakefile ]; then
    echo -n "Generating and fixing $file... "

    file=lily/$outdir/lexer.cc
    rm -f $file
    make conf=$CONF -C lily $outdir/lexer.cc > /dev/null 2>&1 || true

    mv $file $file.orig
    sed -e 's/\<cin\>/std::cin/g' \
	-e 's/\<cout\>/std::cout/g' \
	-e 's/\<cerr\>/std::cerr/g' \
	$file.orig > $file
    echo "done"
fi

cat <<EOF

Remove config.cache before rerunning ./configure

Reconfigure, refix, and make doing something like:

    rm -f config.cache
    CPPFLAGS=-I$(pwd)/lily/$outdir ./configure $ENABLECONFIG
    $SETCONF$0
    make $setconf
EOF
