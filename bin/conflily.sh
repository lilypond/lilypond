#!/bin/sh
# conflily
# ugh: must be executed from lilypond-x.x.x (bin/conflily)

# the lilypond root dir looks like this:
# current -> ./lilypond-x.x.x  symlink to current source
# lilypond-x.x.x               the lilypond source
# patches                      gzipped patches
# releases                     gzipped releases

# the lily root dir
#
lelie=$HOME/music/lily
#

if [ ! -e $HOME/lelie ]; then
	ln -s $lelie $HOME/lelie
fi

rm $lelie/current 2>&1 > /dev/null
current=`basename \`pwd\``
echo ln -s $lelie/$current $lelie/current
ln -s $lelie/$current $lelie/current

ln -sf $lelie/current/lily/out/lilypond bin/out/lilypond
ln -sf $lelie/current/mi2mu/out/mi2mu bin/out/mi2mu

if [ "x$LILYINCLUDE" = "x" ]; then
	echo you should make add the following to your login script
	echo "export LILYINCLUDE=$lelie/current/init:$lelie/current/input:$lelie/current/mutopia:$lelie/current/mutopia/j.s.bach"
	echo "export PATH=$PATH:$lelie/current/bin/out/"
	echo "export MFINPUTS=$MFINPUTS:$lelie/current/mf"
fi

configure --prefix=/usr --enable-debugging --enable-printing --enable-checking

