#!@BASH@
# make-cygwin-cross
#
# Build and install cross-development tools for cygwin package 
# (binutils, compiler, flex, bison).
# Using this cross-development enviroment, build and install
# native cygwin packages (guile, lilypond).
#
# Besides being a handy script for me, currently this mainly serves
# as documentation for cross-building lilypond.
#
# To use this script, you need
#
#  * all development tools to build a native LilyPond, see INSTALL.txt
#
#  * pre-release cygnus sources (and a binary) from:
#
#      ftp://sourceware.cygnus.com/pub/cygwin/private/cygwin-net-485/
#
#          binutils/binutils-19990818-1-src.tar.gz
#          bison/bison-src.tar.gz
#          cygwin/cygwin-20000301-src.tar.gz
#          cygwin/cygwin-20000301.tar.gz
#          flex/flex-src.tar.gz
#          gcc/gcc-2.95.2-1-src.tar.gz
#
#  * guile-1.3.4.tar.gz
#
#  * lilypond-1.3.38.jcn1.tar.gz
#
#  * lots of disk space, ca 353MB
#

################
# config section
################

ROOT=/usr/src/cygwin-net-485
PREFIX=$ROOT/usr
NATIVE_PREFIX=/Cygnus/usr

# we want to install LilyPond under
# /Cygnus/lilypond-x.y.z
if [ ! -e $NATIVE_PREFIX ]; then
	echo "$NATIVE_PREFIX: no such directory"
	echo
	echo "do:"
	echo "        mkdir -p $ROOT/$NATIVE_PREFIX"
	echo "        ln -s $ROOT/Cygnus /Cygnus"
	exit 1
fi

# urg
DEVEL=/home/fred
WWW=$DEVEL/WWW/lilypond/gnu-windows

CYGWIN_SOURCE=$DEVEL/sourceware.cygnus.com/pub/cygwin/private/cygwin-net-485
SOURCE_PATH=$DEVEL/usr/src/releases:$DEVEL/usr/src/patches:$DEVEL/usr/src/lilypond/Documentation/ntweb

HOST=`uname -m`-gnu-`uname -s | tr '[A-Z]' '[a-z]'`

cygwin_binary=cygwin-20000301.tar.gz
TARGET_ARCH=i686-pc-cygwin
CYGWIN_DLL=cygwin1-net-485.dll

cross_packages="
binutils-19990818
gcc-2.95.2
cygwin
flex
bison
"

cross_configure='--prefix=$PREFIX --target=$TARGET_ARCH'
native_configure='--prefix=$PREFIX --target=$TARGET_ARCH'
##native_configure='--prefix=$NATIVE_PREFIX --target=$TARGET_ARCH'
gcc_make='LANGUAGES="c++"'

native_configure='--target=$TARGET_ARCH --build=$TARGET_ARCH --host=$HOST --oldincludedir=$PREFIX/include'

guile_patch='guile-1.3.4-gnu-windows.patch'
guile_configure='--prefix=$PREFIX --enable-sizeof-int=4 --enable-sizeof-long=4 --enable-restartable-syscalls=yes'
guile_make='oldincludedir=$PREFIX/include'

lilypond_version=@TOPLEVEL_VERSION@
lilypond_prefix="$NATIVE_PREFIX/lilypond-$lilypond_version"
lilypond_ldflags='-L$PREFIX/lib -lguile $PREFIX/bin/$CYGWIN_DLL'
lilypond_configure='--prefix=$lilypond_prefix'
## URG, help2man: doesn't know about cross-compilation.
#lilypond_make='-k && make -k && make -k'
lilypond_patch=lilypond-manpages.patch

native_packages="
guile
lilypond-$lilypond_version
"


#######################
# end of config section
#######################

cygwin_dirs=`/bin/ls -d1 $CYGWIN_SOURCE/*`
cygwin_source_path=`echo $CYGWIN_SOURCE $cygwin_dirs`
source_path=`echo $SOURCE_PATH:$cygwin_source_path | sed 's/:/ /g'`

# mingw doesn't work yet. 
#TARGET_ARCH=i686-pc-mingw32
#
# building cross-compiler i686-pc-mingw32-gcc exists on:
#
#  /usr/src/cygwin-mingw/usr/src/cross-gcc-2.95.2/gcc/xgcc -B/usr/src/cygwin-mingw/usr/src/cross-gcc-2.95.2/gcc/ -B/usr/src/cygwin-mingw/usr/i686-pc-mingw32/bin/ -I/usr/src/cygwin-mingw/usr/i686-pc-mingw32/include -O2 -I../../gcc-2.95.2/gcc/../winsup/include -DCROSS_COMPILE -DIN_GCC     -g -O2 -I./include   -g1  -DIN_LIBGCC2 -D__GCC_FLOAT_NOT_NEEDED   -I. -I../../gcc-2.95.2/gcc -I../../gcc-2.95.2/gcc/config -I../../gcc-2.95.2/gcc/../include -c
#
# with:
#
#  ../../gcc-2.95.2/gcc/libgcc2.c:2582: conflicting types for `getpagesize'
#/usr/src/cygwin-mingw/usr/i686-pc-mingw32/include/sys/unistd.h:43: previous declaration of `getpagesize'
# it seems that __CYGWIN__ is not defined in mingw compiler/or compiler isn't
# fully ported to mingw

###########
# functions
###########

untar ()
{(
	set -x
	tarball=$1
	dest_dir=$2

	first_dir=`tar tzf $tarball | head -1`
	src_dir=`dirname $first_dir`

	if [ "$src_dir" = "src" -o "$src_dir" = "./src" \
	  -o "$src_dir" = "." ]; then
		src_dir=$first_dir
	fi

	tar xzf $tarball

	if [ "$src_dir" != "$dest_dir" -a "$src_dir" != "$dest_dir/" ]; then
		mv $src_dir $dest_dir
		rm -rf ./$first_dir
	fi
)
}

build ()
{(
	set -
	package=$1

	if [ -d $package ]; then
		echo "./$package: directory exists"
		echo "$package: skipping"
		exit 0
	fi

        name=`echo $package | sed 's/-.*//'`
        name_patch=`eval echo $\`eval echo ${name}_patch\``
        name_make=`eval echo $\`eval echo ${name}_make\``
        name_configure=`eval echo $\`eval echo ${name}_configure\``
        name_ldflags=`eval echo $\`eval echo ${name}_ldflags\``
        type_configure=`eval echo $\`eval echo ${type}_configure\``

	find_path $package*src.tar.gz
	if [ "$found" = "" ]; then
		find_path $package.tar.gz
	fi
	if [ "$found" = "" ]; then
		find_path $name*tar.gz
	fi
	if [ "$found" = "" ]; then
		echo "$package: no such tarball"
		exit 1
	fi

	untar $found $package
	if [ "x$name_patch" != "x" ]; then
		(
		cd $package
		find_path $name_patch
		set -x
		if [ "$found" = "" ]; then
			echo "$name_patch: no such file"
			exit 1
		fi
		patch -p1 -E < $found
		)
	fi
	set -x
	mkdir $type-$package
	cd $type-$package

	rm -f config.cache
	LDFLAGS="`eval echo $name_ldflags`" ../$package/configure `eval echo $type_configure $name_configure` &&
	make `eval echo $name_make` && 
	make install `eval echo $name_make`
)
}

find_path ()
{
	set -
	expr=$1
	for i in $source_path; do
		found=`/bin/ls -d1 $i/$expr 2>/dev/null | head -1`
		if [ -e "$found" ]; then
			return
		fi
	done
	found=
}
##################
# end of functions
##################

#
# setup
#

set -x
mkdir -p $ROOT
if [ ! -d $PREFIX ]; then
	cd $ROOT
	find_path $cygwin_binary
	if [ "$found" = "" ]; then
		echo "$cygwin_binary: no such tarball"
		exit 1
	fi
	set -x
	tar xzf $found
	# urg, bug in gcc's cross-make configuration
	mkdir -p $PREFIX/lib/gcc-lib/$TARGET_ARCH/2.95.2

	cd $PREFIX
	# urg, bug in gcc's cross-make configuration
	if [ ! -e include ]; then
		#mv include huh-include
		# not target-arch, but [i686-]pc-cygwin!
		ln -s $PREFIX/i686-pc-cygwin/include .
		# these necessary only for mingw32
		ln -s $PREFIX/i686-pc-cygwin/include $TARGET_ARG
		ln -s $PREFIX/include $PREFIX/$TARGET_ARCH/include
	fi
else
	echo "$PREFIX: already exists"
	echo "$cygwin_binary: skipping"
fi

mkdir -p $PREFIX/src
cd $PREFIX/src
PATH=$PATH:$PREFIX/bin

type=cross
for i in $cross_packages; do
	if build $i; then
		true
	else
		echo "$i: build failed"
		exit 1
	fi
done

# urg, bug in gcc's cross-make install
ln -f $PREFIX/bin/cygwin1.dll $PREFIX/bin/$CYGWIN_DLL
ln -f $PREFIX/bin/$TARGET_ARCH-gcc $PREFIX/$TARGET_ARCH/bin/cc 
ln -f $PREFIX/bin/$TARGET_ARCH-c++ $PREFIX/$TARGET_ARCH/bin/c++
ln -f $PREFIX/bin/$TARGET_ARCH-g++ $PREFIX/$TARGET_ARCH/bin/g++

PATH=$PREFIX/$TARGET_ARCH/bin:$PREFIX/bin:$PATH 

cd $PREFIX/src
mkdir -p $PREFIX/src

type=native
for i in $native_packages; do
	if build $i; then
		true
	else
		echo "$i: build failed"
		exit 1
	fi
done

lilypond_name=lilypond-$lilypond_version
rm -f $WWW/$lilypond_name.zip
cd $lilypond_prefix/.. && zip -r $WWW/$lilypond_name.zip $lilypond_name
rm -f $WWW/$CYGWIN_DLL.zip
cd $PREFIX/bin && zip $WWW/$CYGWIN_DLL.zip $CYGWIN_DLL
