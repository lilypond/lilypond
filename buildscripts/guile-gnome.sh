#!@BASH@
# guile-gnome.sh -- download, compile, install g-wrap, guile-gnome TLA and
# pango CVS

# LilyPond has an experimental gnome canvas output backend -- hackers
# only.  This depends on unreleased version of guile-gnome, which
# depends on an unreleased, forked version of g-wrap.  We also need
# pango CVS > 2004-06-12

# Note: this install information is volatile, you'll probably want to
# pull all from from guile-gnome-devel@gnu.org--2004 soon.

set -ex

# Where user built stuff will be installed
OPT=$HOME/usr/pkg

if [ -x /usr/bin/gcc34 ] ;then
    export GCC=gcc34
fi

if [ -x /usr/bin/gcc-3.4 ] ;then
    export GCC=gcc-3.4
fi

export AUTOMAKE=automake-1.8
export AUTOCONF=$(which autoconf2.50)

if [ -z "$AUTOCONF" ]; then
    unset AUTOCONF
fi


# test: the name of our download and build directory
rm -rf test
mkdir test
cd test

## 1.  install gnome-devel
##     - Debian/unstable: apt-get install gnome-devel
##     - ...

## 2.  get pango CVS

mkdir -p gnome/CVS
cd gnome
echo ":pserver:anonymous@anoncvs.gnome.org:/cvs/gnome" > CVS/Root
echo "." > CVS/Repository
cvs -z3 checkout -P pango
cd pango
rm -rf $OPT/pango
./autogen.sh --help
./configure --prefix=$OPT/pango --enable-maintainer-mode --enable-gtk-doc
make XFT_LIBS="-L/usr/lib -lXft -L/usr/X11R6/lib -lfreetype -lz -lXrender -lX11 -lfontconfig" install

cd ../..

export PKG_CONFIG_PATH=$OPT/pango/lib/pkgconfig:$PKG_CONFIG_PATH


## 3.  *** NOTE: use guile-1.6 for g-wrap and guile-gnome ***
## using GUILE CVS g-wrap/guile-gnome is experimental (read: segfaults)
## Assuming that system has guile-1.6 installed in /usr/bin 
PATH=/usr/bin:$PATH

if [ -d $OPT/libffi/ ]; then
    export LDFLAGS=-L$OPT/libffi/lib
    export CPPFLAGS=-I$OPT/libffi/include
fi

## 4.  get g-wrap 2.0
tla register-archive a.rottmann@gmx.at--2004-main \
    http://people.debian.org/~rotty/arch/a.rottmann@gmx.at/2004-main || true

rm -rf g-wrap
if false; then
    ## pull latest g-wrap from janneke -- this step is probably no longer
    ## necessary when you read this
    tla register-archive janneke@gnu.org--2004-gnome \
	http://lilypond.org/~janneke/{arch}/2004-gnome || true
    tla get janneke@gnu.org--2004-gnome/g-wrap--janneke g-wrap
else
    ## tla get a.rottmann@gmx.at--2004-main/g-wrap--tng g-wrap
    tla get a.rottmann@gmx.at--2004-main/g-wrap--mainline--1.9.0 g-wrap
fi
cd g-wrap

rm -rf $OPT/g-wrap
sh autogen.sh --noconfigure
mkdir =build
cd =build
../configure --prefix=$OPT/g-wrap
make install

# not a good idea
## cp srfi-34.scm from CVS head ?  --hwn
#(cd $OPT/g-wrap/share/guile/site
# mv srfi-34.scm srfi-34.scm-g-wrap
# cp $OPT/guile/share/guile-1.7/srfi/srfi-34.scm .)

cd ../..

## 5.  get guile-gnome
tla register-archive guile-gnome-devel@gnu.org--2004 \
    http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/ || true
rm -rf guile-gnome
tla get guile-gnome-devel@gnu.org--2004/dists--dev guile-gnome
cd guile-gnome
tla build-config -r configs/gnu.org/dev
cd src

## 6.  get the gnome canvas module
tla get guile-gnome-devel@gnu.org--2004/libgnomecanvas--dev libgnomecanvas

rm -rf $OPT/guile-gnome
sh autogen.sh --noconfigure
mkdir ../=build
cd ../=build

export GUILE_LOAD_PATH=$OPT/g-wrap/share/guile/site:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$OPT/g-wrap/lib:$LD_LIBRARY_PATH
PKG_CONFIG_PATH=$OPT/g-wrap/lib/pkgconfig:$PKG_CONFIG_PATH

../src/configure --prefix=$OPT/guile-gnome
make install CC=$GCC G_WRAP_MODULE_DIR=$OPT/g-wrap/share/guile/site

GUILE_LOAD_PATH=$OPT/guile-gnome/share/guile:$GUILE_LOAD_PATH
LD_LIBRARY_PATH=$OPT/guile-gnome/lib:$LD_LIBRARY_PATH

# simple test
guile -s ../src/libgnomecanvas/examples/canvas.scm
