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

# urg
DEVEL=/home/fred
WWW=$DEVEL/WWW/lilypond/gnu-windows
#WWW=/tmp

CYGWIN_SOURCE=$DEVEL/sourceware.cygnus.com/pub/cygwin/private/cygwin-net-485
SOURCE_PATH=$DEVEL/usr/src/releases:$DEVEL/usr/src/patches:$DEVEL/usr/src/lilypond/Documentation/ntweb

HOST=`uname -m`-gnu-`uname -s | tr '[A-Z]' '[a-z]'`

cygwin_binary=cygwin-20000301.tar.gz
TARGET_ARCH=i686-pc-cygwin
CYGWIN_DLL=cygwin1-net-485.dll

cross_packages="
binutils-19990818
gcc-2.95.2
flex
bison
"

not_yet_needed=cygwin-2000301

cross_configure='--prefix=$PREFIX --target=$TARGET_ARCH'
##native_configure='--prefix=$NATIVE_PREFIX --target=$TARGET_ARCH'
gcc_make='LANGUAGES="c++"'
cygwin_make='-k || true'

native_configure='--target=$TARGET_ARCH --build=$TARGET_ARCH --host=$HOST --oldincludedir=$PREFIX/include --prefix=$NATIVE_PREFIX'

guile_patch='guile-1.3.4-gnu-windows.patch'
guile_configure='--enable-sizeof-int=4 --enable-sizeof-long=4 --enable-restartable-syscalls=yes'
guile_make='oldincludedir=$PREFIX/include'

lilypond_version=@TOPLEVEL_VERSION@
#lilypond_prefix="$NATIVE_PREFIX/lilypond-$lilypond_version"
lilypond_ldflags='-L$PREFIX/lib -lguile $PREFIX/bin/$CYGWIN_DLL'
#lilypond_configure='--prefix=$lilypond_prefix'
## URG, help2man: doesn't know about cross-compilation.
#lilypond_make='-k || make -k || true'
lilypond_patch=lilypond-manpages.patch
lilypond_install='--just-print'

native_packages="
guile-1.3.4
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

expand ()
{(

	set -
	string=`eval echo $\`eval echo ${1}${2}\``
	eval echo $string
)
}

build ()
{(
	package=$1
	set -
	if [ -d $package ]; then
		echo "$package: directory exists"
		echo "$package: skipping"
		exit 0
	fi

        name=`echo $package | sed 's/-.*//'`
        name_patch=`expand $name _patch`
        name_ldflags=`expand $name _ldflags`
        name_configure=`expand $name _configure`
        type_configure=`expand $type _configure`
        name_make=`expand $name _make`
        name_install=`expand $name _install`

	found=`find_path $package*src.tar.gz`
	if [ "$found" = "" ]; then
		found=`find_path $package.tar.gz`
	fi
	if [ "$found" = "" ]; then
		found=`find_path $name*tar.gz`
	fi
	if [ "$found" = "" ]; then
		echo "$package: no such tarball"
		exit 1
	fi
	
	untar $found $package
	if [ "x$name_patch" != "x" ]; then
		(
		cd $package
		found=`find_path $name_patch`
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
	LDFLAGS="$name_ldflags" ../$package/configure $type_configure $name_configure || exit 1
	make $name_make || exit 1
	make install $name_install || exit 1
)
}

## urg, let's hope Cygnus uses rpm for packaging its next release
pack ()
{(
	set -
	package=$1
        name=`echo $package | sed 's/-.*//'`
        name_pack_install=`expand $name _pack_install`
	install_root=/tmp/$package-install
	install_prefix=$install_root/$NATIVE_PREFIX

	set -x
	rm -rf $install_root
	mkdir -p $install_prefix

	cd $PREFIX/src/$type-$package || exit 1
	make install prefix=$install_prefix $name_pack_install

        ## duh, rename executables,
	## for people that use a dumb shell instead of bash
	cd $install_prefix/bin &&
	for i in `/bin/ls -d1 *`; do
		base=`basename $i .exe`
		if [ $base.exe != $i ]; then
			type="`file $i`"
			if expr "$type" : '.*Windows.*\(executable\).*'; then
				mv -f $i $base.exe
			fi
		fi
	done
        rm -f $WWW/$package.zip
        cd $install_root && zip -r $WWW/$package.zip .$NATIVE_PREFIX
)
}

find_path ()
{(
	set -
	expr=$1
	found=
	for i in $source_path; do
		found=`/bin/ls -d1 $i/$expr 2>/dev/null | head -1`
		if [ -e "$found" ]; then
			break
		fi
	done
	echo $found
)
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
	found=`find_path $cygwin_binary`
	if [ "$found" = "" ]; then
		echo "$cygwin_binary: no such tarball"
		exit 1
	fi
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
		ln -s $PREFIX/i686-pc-cygwin/include $TARGET_ARCH
		ln -s $PREFIX/include $PREFIX/$TARGET_ARCH/include
	fi
else
	echo "$PREFIX: already exists"
	echo "$cygwin_binary: skipping"
fi

if [ ! -e $NATIVE_PREFIX ]; then
	ln -s $ROOT /Cygnus || exit 1
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
mv -f $PREFIX/bin/cygwin1.dll $PREFIX/bin/$CYGWIN_DLL
ln -f $PREFIX/bin/$TARGET_ARCH-gcc $PREFIX/$TARGET_ARCH/bin/cc 
ln -f $PREFIX/bin/$TARGET_ARCH-c++ $PREFIX/$TARGET_ARCH/bin/c++
ln -f $PREFIX/bin/$TARGET_ARCH-g++ $PREFIX/$TARGET_ARCH/bin/g++

PATH=$PREFIX/$TARGET_ARCH/bin:$PREFIX/bin:$PATH 

mkdir -p $PREFIX/src
cd $PREFIX/src

set -x
type=native
for i in $native_packages; do
	if build $i; then
		pack $i
		continue
	fi
	exit 1
done

rm -f $WWW/$CYGWIN_DLL.zip
cd $PREFIX/bin && zip $WWW/$CYGWIN_DLL.zip $CYGWIN_DLL
