#!@BASH@
# guile-gnome.sh -- download, compile, install guile-gnome

# LilyPond has an experimental gnome canvas output backend -- hackers
# only.  This depends on unreleased version of guile-gnome, which
# depends on an unreleased, forked version of g-wrap.

# Note: this install information is volatile, you'll probably want to
# pull all from from guile-gnome-devel@gnu.org--2004 soon.

set -ex

if  [ -d $HOME/usr/pkg/libffi/ ]; then
    export LDFLAGS=-L$HOME/usr/pkg/libffi/lib
    export CPPFLAGS=-I$HOME/usr/pkg/libffi/include
fi

if [ -x /usr/bin/gcc34 ] ;then
    export GCC=gcc34
fi

if [ -x /usr/bin/gcc-3.4 ] ;then
    export GCC=gcc-3.4
fi

export AUTOMAKE=automake-1.8
export AUTOCONF=`which autoconf2.50 `

if [ "" = "$AUTOCONF" ] ; then
 unset AUTOCONF
fi


# test: the name of our download and build directory
rm -rf test
mkdir test
cd test

## 1.  install gnome-devel
##     - Debian/unstable: apt-get install gnome-devel
##     - ...


## 2.  *** NOTE: use guile-1.6 for g-wrap and guile-gnome ***
## using GUILE CVS g-wrap/guile-gnome is experimental (read: segfaults)
## Assuming that system has guile-1.6 installed in /usr/bin 
PATH=/usr/bin:$PATH

## 3.  get g-wrap 2.0
tla register-archive a.rottmann@gmx.at--2004-main \
    http://people.debian.org/~rotty/arch/a.rottmann@gmx.at/2004-main || true

rm -rf g-wrap
## tla get a.rottmann@gmx.at--2004-main/g-wrap--tng g-wrap
## pull latest g-wrap from janneke -- this step is probably no longer
## necessary when you read this
tla register-archive janneke@gnu.org--2004-gnome \
    http://lilypond.org/~janneke/{arch}/2004-gnome || true
tla get janneke@gnu.org--2004-gnome/g-wrap--janneke g-wrap
cd g-wrap

rm -rf $HOME/usr/pkg/g-wrap
sh autogen.sh --noconfigure
mkdir =build
cd =build
../configure --prefix=$HOME/usr/pkg/g-wrap
make install

# cp srfi-34.scm from CVS head ?  --hwn
(cd $HOME/usr/pkg/g-wrap/share/guile/site
 mv srfi-34.scm srfi-34.scm-g-wrap
 cp $HOME/usr/pkg/guile/share/guile-1.7/srfi/srfi-34.scm .)

cd ../..

## 4.  get guile-gnome
tla register-archive guile-gnome-devel@gnu.org--2004 \
    http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/ || true
rm -rf guile-gnome
tla get guile-gnome-devel@gnu.org--2004/dists--dev guile-gnome
cd guile-gnome
tla build-config -r configs/gnu.org/dev
cd src

## 5.  get the gnome canvas module
tla get guile-gnome-devel@gnu.org--2004/libgnomecanvas--dev libgnomecanvas

rm -rf $HOME/usr/pkg/guile-gnome
sh autogen.sh --noconfigure
mkdir ../=build
cd ../=build

export GUILE_LOAD_PATH=$HOME/usr/pkg/g-wrap/share/guile/site:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/g-wrap/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/usr/pkg/g-wrap/lib/pkgconfig:$PKG_CONFIG_PATH

# ugh
# pango CVS wants libtool 1.5.6 (barfs with 1.6.0)
# guile-gnome wants libtool 1.6.0 (barfs with 1.5.6)
if  [ -d $HOME/usr/pkg/pango/ ]; then
    export PKG_CONFIG_PATH=$HOME/usr/pkg/pango/lib/pkgconfig:$PKG_CONFIG_PATH
    export LDFLAGS=-L$HOME/usr/pkg/pango/lib
    export CPPFLAGS=-I$HOME/usr/pkg/pango/include/pango-1.0
fi 

../src/configure --prefix=$HOME/usr/pkg/guile-gnome

G_WRAP_MODULE_DIR=$HOME/usr/pkg/g-wrap/share/guile/site make install CC=$GCC

export GUILE_LOAD_PATH=$HOME/usr/pkg/guile-gnome/share/guile:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/guile-gnome/lib:$LD_LIBRARY_PATH

# simple test
guile -s ../src/libgnomecanvas/examples/canvas.scm
