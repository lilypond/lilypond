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
#      ftp.xraylith.wisc.edu/pub/khan/gnu-win32/mingw32/runtime/
#
#          bin-crtdll-2000-02-03.tar.gz  (mingw only)
#
#  * rx-1.5.tar.gz
#
#  * guile-1.3.4.tar.gz
#
#  * lilypond-1.3.38.jcn1.tar.gz
#
#  * lots of disk space, ca 353MB
#
# Not yet needed:
#
#  * db-2.7.7.tar.gz
#
#  * rpm-3.0.4.tar.gz 
#

################
# config section
################

target=cygwin
#target=mingw

if [ $target = cygwin ]; then
	ROOT=/usr/src/cygwin-net-485
	TARGET_ARCH=i686-pc-cygwin
else
	ROOT=/usr/src/mingw-net-485
	TARGET_ARCH=i386-pc-mingw32
fi
PREFIX=$ROOT/usr
NATIVE_PREFIX=/Cygnus/usr

# urg
DEVEL=/home/fred
WWW=$DEVEL/WWW/lilypond/gnu-windows
#WWW=/tmp

CYGWIN_SOURCE=$DEVEL/sourceware.cygnus.com/pub/cygwin/private/cygwin-net-485
MINGW_SOURCE=$DEVEL/ftp.xraylith.wisc.edu/pub/khan/gnu-win32/mingw32/runtime
SOURCE_PATH=$DEVEL/usr/src/releases:$DEVEL/usr/src/patches:$DEVEL/usr/src/lilypond/Documentation/ntweb:$MINGW_SOURCE:/usr/src/redhat/SOURCES

HOST=`uname -m`-gnu-`uname -s | tr '[A-Z]' '[a-z]'`

cygwin_binary=cygwin-20000301.tar.gz
mingw_binary=bin-crtdll-2000-02-03.tar.gz

#CYGWIN_DLL=cygwin1-net-485.dll
CYGWIN_DLL=cygwin1.dll
MINWG_DLL=mingwc10-net-485.dll


################
# cross packages
################

cross_packages="
binutils-19990818
gcc-2.95.2
flex
bison
"

not_yet_needed="
cygwin-2000301
"

cross_configure='--prefix=$PREFIX --target=$TARGET_ARCH'
gcc_make='LANGUAGES="c++"'
cygwin_make='-k || true'

#################
# native packages
#################

# Typically, we install native packages under
#
#   /Cygnus/usr/package-x.y.z
#
# so that's how we configure them.
#
native_configure='--target=$TARGET_ARCH --build=$TARGET_ARCH --host=$HOST --oldincludedir=$PREFIX/include --prefix=$NATIVE_PREFIX/$package --program-suffix='
native_config_site='$PREFIX/share/native-config.site'

rx_install='prefix=$PREFIX'

db_patch='db-2.7.7-cygwin.patch'
db_configure='urg'
db_install='prefix=$PREFIX'
rpm_patch='rpm-3.0.4-cygwin.patch'
rpm_install='prefix=$PREFIX'

guile_patch='guile-1.3.4-gnu-windows.patch'
if [ $target = mingw ]; then
	guile_patch1='guile-1.3.4-mingw.patch'
	guile_cflags='-I $PREFIX/$TARGET_ARCH/include -I $PREFIX/i686-pc-cygwin/include'
fi
guile_ldflags='-L$PREFIX/lib $PREFIX/bin/$CYGWIN_DLL'
guile_make='oldincludedir=$PREFIX/include'

# We need to get guile properly installed for cross-development, ie
# at our prefix: $PREFIX.  When packaging, the prefix we configured
# for, will be used.
#
guile_install='prefix=$PREFIX'

lilypond_version=@TOPLEVEL_VERSION@
if [ $target = mingw ]; then
	lilypond_cflags='-I $PREFIX/$TARGET_ARCH/include -I $PREFIX/i686-pc-cygwin/include'
fi
lilypond_ldflags='-L$PREFIX/lib -lguile $PREFIX/bin/$CYGWIN_DLL'
lilypond_configure='--enable-tex-tfmdir=/texmf/fonts/tfm/public/cm'
## URG, help2man: doesn't know about cross-compilation.
#lilypond_make='-k || make -k || true'
lilypond_patch=lilypond-manpages.patch
# Don't install lilypond
lilypond_install='--just-print'
lilypond_before_zip='cp -pr $PREFIX/src/$package/input $install_prefix \; cp -p \`find $PREFIX/src/$package -type f -maxdepth 1\` $install_prefix'

native_packages="
guile-1.3.4
lilypond-$lilypond_version
"

not_yet_needed="
rx-1.5
db-2.7.7
rpm-3.0.4
"

#######################
# end of config section
#######################

cygwin_dirs=`/bin/ls -d1 $CYGWIN_SOURCE/*`
cygwin_source_path=`echo $CYGWIN_SOURCE $cygwin_dirs`
source_path=`echo $SOURCE_PATH:$cygwin_source_path | sed 's/:/ /g'`

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
	string=`eval echo $\`eval echo "${1}${2}"\``
	eval echo $string
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

fix_extension ()
{
	file=$1
	ext=$2
	expr="$3"
	base=`basename $file $ext`
	if [ $base$ext != $i ]; then
		type="`file $file`"
		if expr "$type" : "$expr"; then
			mv -f $file $base$ext
		fi
	fi
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
        name_cflags=`expand $name _cflags`
        name_ldflags=`expand $name _ldflags`
        name_configure=`expand $name _configure`
        type_config_site=`expand $type _config_site`
        type_configure=`expand $type _configure`
        name_make=`expand $name _make`
        name_before_install="`expand $name _before_install`"
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
	patch=`expand $name _patch`
	count=0
	while [ "x$patch" != "x" ]; do
		(
		cd $package
		set -x
		found=`find_path $patch`
		if [ "$found" = "" ]; then
			echo "$patch: no such file"
			exit 1
		fi
		patch -p1 -E < $found
		)
		count=`expr $count + 1`
		patch=`expand $name _patch$count`
	done
	mkdir $type-$package
	cd $type-$package

	rm -f config.cache
	CONFIG_SITE="$type_config_site" CFLAGS="$name_cflags" LDFLAGS="$name_ldflags" ../$package/configure $type_configure $name_configure || exit 1
	make $name_make || exit 1
	`eval $name_before_install` || exit 1
	make install $name_install || exit 1
)
}

## urg, let's hope Cygnus uses rpm for packaging its next release
pack ()
{(
	set -
	package=$1

	zip=$WWW/$package.zip
	if [ -e $zip ]; then
		echo "$zip: package exists"
		echo "$zip: skipping"
		exit 0
	fi

        name=`echo $package | sed 's/-.*//'`
        name_pack_install=`expand $name _pack_install`
	install_root=/tmp/$package-install
	install_prefix=$install_root/$NATIVE_PREFIX/$package
	name_before_zip=`expand $name _before_zip`

	set -x
	rm -rf $install_root
	mkdir -p $install_prefix

	cd $PREFIX/src/$type-$package || exit 1
	make install prefix=$install_prefix $name_pack_install

        ## duh, rename executables,
	## for people that use a dumb shell instead of bash
	cd $install_prefix/bin &&
	for i in `/bin/ls -d1 *`; do
		fix_extension $i .exe '.*Windows.*\(executable\).*'
		fix_extension $i .py '.*\(python\).*'
	done

        rm -f $zip
	`eval $name_before_zip` || exit 1
        cd $install_root && zip -ry $zip .$NATIVE_PREFIX
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
	rm -f include
	ln -s $PREFIX/$TARGET_ARCH/include .
else
	echo "$PREFIX: already exists"
	echo "$cygwin_binary: skipping"
fi

# mingw
if [ ! -d $PREFIX/$TARGET_ARCH ]; then
	cd $PREFIX
	found=`find_path $mingw_binary`
	if [ "$found" = "" ]; then
		echo "$mingw_binary: no such tarball"
		exit 1
	fi
	tar xzf $found

	mkdir -p $PREFIX/lib/gcc-lib/$TARGET_ARCH/2.95.2
	rm -f include
	mkdir -p $PREFIX/i386-mingw32/include
	ln -s $PREFIX/i386-mingw32 $TARGET_ARCH
	ln -s $PREFIX/$TARGET_ARCH/include .
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

# urg, bug in binutil's cross-make install
ln -f $PREFIX/bin/$TARGET_ARCH-objdump $PREFIX/$TARGET_ARCH/bin/objdump

# urg, bug in gcc's cross-make install
mv $PREFIX/bin/cygwin1.dll $PREFIX/bin/$CYGWIN_DLL
mv $PREFIX/bin/mingwc10.dll $PREFIX/bin/$MINGW_DLL
ln -f $PREFIX/bin/$TARGET_ARCH-gcc $PREFIX/$TARGET_ARCH/bin/cc 
ln -f $PREFIX/bin/$TARGET_ARCH-c++ $PREFIX/$TARGET_ARCH/bin/c++
ln -f $PREFIX/bin/$TARGET_ARCH-g++ $PREFIX/$TARGET_ARCH/bin/g++

PATH=$PREFIX/$TARGET_ARCH/bin:$PREFIX/bin:$PATH 

mkdir -p $PREFIX/src
cd $PREFIX/src

ncs=`eval echo $native_config_site`
rm -f $ncs
mkdir -p `dirname $ncs`
cat > $ncs <<EOF
ac_cv_sizeof_int=4
ac_cv_sizeof_long=4
ac_cv_sys_restartable_syscalls=yes
ac_cv_sprintf_count=yes
ac_cv_spinlocks=no
db_cv_sprintf_count=yes
db_cv_spinlocks=no
EOF

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
