#!@BASH@

# WIP - this (the guile-gnome canvas) is broken ATM.

# guile-gnome.sh -- download, compile, install g-wrap, guile-gnome,
# pango

# LilyPond has an experimental gnome canvas output backend -- hackers
# only.  This depends on rather new versions of guile-gnome, g-wrap
# and pango.

set -ex

# Where user built stuff will be installed
OPT=$HOME/usr/pkg

# What extra modules to pull (eg: EXTRA="libgnomecanvas libwnck")
EXTRA=${EXTRA-libgnomecanvas}

export AUTOMAKE=automake-1.8
export ACLOCAL=aclocal-1.8
export AUTOCONF=$(which autoconf2.50)
export AUTOHEADER=$(which autoheader2.50)

export PKG_CONFIG_PATH
export LD_LIBRARY_PATH
export GUILE_LOAD_PATH

if [ -z "$AUTOCONF" ]; then
    unset AUTOCONF
fi
if [ -z "$AUTOHEADER" ]; then
    unset AUTOHEADER
fi


# test: the name of our download and build directory
rm -rf test
mkdir test
cd test

## 1.  install gnome-devel
##     - Debian/unstable: apt-get install gnome-devel
##     - ...

## 2.  get pango CVS

PKG_CONFIG_PATH=$OPT/pango/lib/pkgconfig:$PKG_CONFIG_PATH
LD_LIBRARY_PATH=$OPT/pango/lib:$LD_LIBRARY_PATH

mkdir -p gnome/CVS
cd gnome
if ! pkg-config --atleast-version=1.5.1 pango; then
    if [ -n "$BLOEDIGE_RAND" ]; then
	echo ":pserver:anonymous@anoncvs.gnome.org:/cvs/gnome" > CVS/Root
	echo "." > CVS/Repository
	cvs -z3 checkout -P pango
    else
        wget ftp://ftp.gtk.org/pub/gtk/v2.5/pango-1.5.2.tar.gz
	tar -zf pango-1.5.2.tar.gz
	ln -s pango-1.5.2 pango
    fi
    cd pango
    rm -rf $OPT/pango
    if [ ! -f configure ]; then
	./autogen.sh --help
    fi
    ./configure --prefix=$OPT/pango --enable-maintainer-mode --enable-gtk-doc
    make XFT_LIBS="-L/usr/lib -lXft -L/usr/X11R6/lib -lfreetype -lz -lXrender -lX11 -lfontconfig" install
    cd ../..
fi

## 3. Currently (2004-9-14) GUILE CVS needs pending patches from
## * http://lists.gnu.org/archive/html/guile-devel/2004-09/msg00057.html
## * http://lists.gnu.org/archive/html/guile-devel/2004-09/msg00062.html
## to be usable with guile-gnome.
## Use system's guile (1.6.4, hopefully) for now.
PATH=/usr/bin:$PATH

if [ -d $OPT/libffi/ ]; then
    export LDFLAGS=-L$OPT/libffi/lib
    export CPPFLAGS=-I$OPT/libffi/include
fi

PKG_CONFIG_PATH=$OPT/g-wrap/lib/pkgconfig:$PKG_CONFIG_PATH
LD_LIBRARY_PATH=$OPT/g-wrap/lib:$LD_LIBRARY_PATH
GUILE_LOAD_PATH=$OPT/g-wrap/share/guile/site:$GUILE_LOAD_PATH

## 4.  get g-wrap 2.0
## note that bleeding edge (2004-9-13) g-wrap breaks guile-gnome.
if ! pkg-config --exact-version=1.9.1 g-wrap-2.0-guile; then
    if [ -n "$BLOEDIGE_RAND" ]; then
	tla register-archive a.rottmann@gmx.at--2004-main \
	    http://people.debian.org/~rotty/arch/a.rottmann@gmx.at/2004-main || true

        ## tla get a.rottmann@gmx.at--2004-main/g-wrap--tng g-wrap
        ## tla get a.rottmann@gmx.at--2004-main/g-wrap--mainline--1.9.0 g-wrap
	tla get a.rottmann@gmx.at--2004-main/g-wrap--dev--0 g-wrap
        ## ughr:
	mkdir -p g-wrap/libffi
    else
	wget http://savannah.nongnu.org/download/g-wrap/g-wrap-1.9.1.tar.gz
	tar zxf g-wrap-1.9.1.tar.gz
	ln -s g-wrap-1.9.1 g-wrap
    fi
    cd g-wrap
    
    rm -rf $OPT/g-wrap
    if [ ! -f configure ]; then
	sh autogen.sh --noconfigure
    fi    
    mkdir =build
    cd =build
    ../configure --prefix=$OPT/g-wrap --enable-maintainer-mode
    make install
fi    


# not a good idea
## cp srfi-34.scm from CVS head ?  --hwn
#(cd $OPT/g-wrap/share/guile/site
# mv srfi-34.scm srfi-34.scm-g-wrap
# cp $OPT/guile/share/guile-1.7/srfi/srfi-34.scm .)

cd ../..

PKG_CONFIG_PATH=$OPT/guile-gnome/lib/pkgconfig:$PKG_CONFIG_PATH
LD_LIBRARY_PATH=$OPT/guile-gnome/lib:$LD_LIBRARY_PATH
GUILE_LOAD_PATH=$OPT/guile-gnome/share/guile:$GUILE_LOAD_PATH

## 5.  get guile-gnome
if ! pkg-config --atleast-version=2.5.990 guile-gnome-glib; then
    if [ -n "$BLOEDIGE_RAND" ]; then
	tla register-archive guile-gnome-devel@gnu.org--2004 \
	    http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/ || true
	tla get guile-gnome-devel@gnu.org--2004/dists--dev guile-gnome
	cd guile-gnome
	tla build-config -r configs/gnu.org/dev
	cd src

        # 5a.  get extra modules (gnome canvas)
	for i in $EXTRA; do
	    tla get guile-gnome-devel@gnu.org--2004/$i--dev $i
	done

	if [ ! -f configure ]; then
	    sh autogen.sh --noconfigure
	fi
	cd ..
    else
	wget http://ambient.2y.net/wingo/tmp/guile-gnome-platform-2.5.990.tar.gz
	tar xzf guile-gnome-platform-2.5.990.tar.gz
	ln -s guile-gnome-platform-2.5.990 guile-gnome
	cd guile-gnome
	ln -s . src
    fi
    
    rm -rf $OPT/guile-gnome
    mkdir =build
    cd =build

# Using libtool < 1.6.0 together with gcc-3.4 may trigger this problem:
#
#    If a tag has not been given, and we're using a compiler which is
#    not one of the ones with which libtool was built, attempt to
#    infer the compiler from the first word of the command line passed
#    to libtool.
#
    if [ -z "$GCC34" ]; then
    # Use libtool-1.5.6, gcc-3.{2,3} without -O2,
	CFLAGS='-O -g' ../src/configure --prefix=$OPT/guile-gnome --enable-maintainer-mode
    else
    # or use gcc-3.4 with libtool-1.6.0
	CC=$GCC34 ../src/configure --prefix=$OPT/guile-gnome --enable-maintainer-mode
    fi
    make install G_WRAP_MODULE_DIR=$OPT/g-wrap/share/guile/site
fi

# simple test -- fails atm
# guile -s ../src/libgnomecanvas/examples/canvas.scm
