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
#  * RPM distribution of the cygwin pre-release sources, from:
#
#      http://appel.dyndns.org/lilypond/gnu-windows/redhat/SRPMS/
#
#    The tarballs were fetched from:
#
#      ftp://sourceware.cygnus.com/pub/cygwin/private/cygwin-net-485/
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
NATIVE_ROOT=/Cygnus
NATIVE_PREFIX=$NATIVE_ROOT/usr

# urg
DEVEL=/home/fred
distdir=$DEVEL/WWW/lilypond/gnu-windows/lily-w32
#distdir=/tmp

SOURCE_PATH=$DEVEL/usr/src/releases:$DEVEL/usr/src/redhat/SRPMS

ARCH=`uname -m`
OS=`uname -s | tr '[A-Z]' '[a-z]'`
HOST=$ARCH-gnu-$OS
CROSS_TARGET_ARCH=$ARCH-x-$TARGET_ARCH
cta=$ARCH-x-cygwin
ta=i686-cygwin

cygwin_binary=cygwin-20000301.tar.gz
mingw_binary=bin-crtdll-2000-02-03.tar.gz

#CYGWIN_DLL=cygwin1-net-485.dll
CYGWIN_DLL=cygwin1.dll
MINWG_DLL=mingwc10-net-485.dll


################
# cross packages
################

cross_rpm="
binutils-19990818
bison
flex
gcc-2.95.2
"

#guile_after_rpm='ln -f $NATIVE_PREFIX/bin/i686-redhat-cygwin-guile-config $NATIVE_PREFIX/bin/i686-pc-cygwin-guile-config'

lilypond_version=@TOPLEVEL_VERSION@

native_rpm="
rx-1.5
zlib-1.1.3
db-2.7.7
guile-1.3.4
rpm-3.0.4
lilypond-$lilypond_version
"

#######################
# end of config section
#######################

###########
# functions
###########

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
	source_path=${2:-$SOURCE_PATH}
	path_list=`echo $source_path | sed 's/:/ /g'`

	found=
	for i in $path_list; do
		found=`/bin/ls -d1 $i/$expr 2>/dev/null | head -1`
		if [ "$found" != "" ] && [ -e $found ]; then
			break
		fi
	done
	echo $found
)
}

build ()
{(
	package=$1
        name=`echo $package | sed 's/-.*//'`

	if [ $type = "cross" ]; then
		target_arch=$CROSS_TARGET_ARCH
		a=$cta
	else
		target_arch=$TARGET_ARCH
		a=$ta
	fi

	rpm_package=`find_path $name*.rpm "$topdir/RPMS/$cta:$topdir/RPMS/$ta"`
	if [ "$rpm_package" != "" ]; then
		echo "$rpm_package: package exists"
		echo "$rpm_package: just updating db"
		rpm -ivv --justdb --ignoreos --ignorearch --nodeps --force\
			--dbpath $NATIVE_ROOT/var/lib/rpm \
			$topdir/RPMS/$a/$name*.rpm
		exit 0
	fi

	cd $topdir
	found=`find_path $package*src.rpm`
	if [ "$found" != "" ]; then
		rpm --root $NATIVE_ROOT -ivv $found || exit 1
		rpm --target=$target_arch -ba SPECS/$name*.spec || exit 1
	else
		tarball=`/bin/ls -d1 SOURCES/$package*tar.gz 2>/dev/null | head -1`
		if [ "SPECS/$name.spec" != "" ] && [ "$tarball" != "" ]; then
			rpm --target=$target_arch -ba SPECS/$name.spec || exit 1
		else
			found=`find_path $package*tar.gz`
			if [ "$found" != "" ]; then
				rpm --target=$target_arch -ta $found || exit 1
			else
				echo "$package: no such rpm or tarball"
				exit 1
			fi
		fi
	fi
	name=`echo $package | sed 's/-.*//'`

	rpm -ivv --ignoreos --ignorearch --nodeps --force\
		--dbpath $NATIVE_ROOT/var/lib/rpm \
		$topdir/RPMS/$a/$name*.rpm
)
}

##################
# end of functions
##################

#######
# setup
#######

set -x
mkdir -p $ROOT
if [ ! -d $PREFIX/bin ]; then
	mkdir -p $PREFIX
	cd $PREFIX
	mkdir -p include
	mkdir -p $TARGET_ARCH
	cd $TARGET_ARCH && ln -s ../include .
	cd $ROOT
	found=`find_path $cygwin_binary`
	if [ "$found" = "" ]; then
		echo "$cygwin_binary: no such tarball"
		exit 1
	fi
	tar xzf $found
	# urg, bug in gcc's cross-make configuration
	mkdir -p $PREFIX/lib/gcc-lib/$TARGET_ARCH/2.95.2
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

rm -f $NATIVE_ROOT
if [ ! -e $NATIVE_ROOT ]; then
	ln -s $ROOT $NATIVE_ROOT || exit 1
fi

#mv $PREFIX/bin/cygwin1.dll $PREFIX/bin/$CYGWIN_DLL
#mv $PREFIX/bin/mingwc10.dll $PREFIX/bin/$MINGW_DLL

mkdir -p $ROOT/var/redhat
mkdir -p $PREFIX/src/redhat

native_config_site='$PREFIX/share/native-config.site'

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

cross_rpm_dir=$NATIVE_PREFIX/lib/rpm/$cta
cross_rpm_link=/usr/lib/rpm/$cta
mkdir -p $cross_rpm_dir
rm -f $cross_rpm_link
ln -s $NATIVE_PREFIX/lib/rpm/$cta $cross_rpm_link

native_rpm_dir=$NATIVE_PREFIX/lib/rpm/$ta
native_rpm_link=/usr/lib/rpm/$ta
mkdir -p $native_rpm_dir
rm -f $native_rpm_link
ln -s $NATIVE_PREFIX/lib/rpm/$ta $native_rpm_link

cross_macros=$cross_rpm_dir/macros
native_macros=$native_rpm_dir/macros

base_macros=$NATIVE_PREFIX/lib/rpm/base-macros
rm -f $base_macros
cat > $base_macros <<EOF
%root		/Cygnus
%_prefix	/Cygnus/usr
%prefix		/Cygnus/usr
%native_prefix	/Cygnus/usr

%_topdir	%{native_prefix}/src/redhat
%_sourcedir	%{native_prefix}/src/redhat/SOURCES
%_builddir	%{native_prefix}/src/redhat/BUILD
%_srcrpmdir	%{native_prefix}/src/redhat/SRPMS
%_rpmdir	%{native_prefix}/src/redhat/RPMS

#%DocDir		%{native_prefix}/doc
%DocDir		/Cygnus/usr/doc
%_defaultdocdir /Cygnus/usr/doc

%cygwin_dll	%{native_prefix}/bin/cygwin1.dll

%cflags		%{optflags}

%sourcedir	.
EOF

cp -f $base_macros $cross_macros
cat >> $cross_macros <<EOF
%_rpmfilename	%%{ARCH}-x-cygwin/%%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}-x-cygwin.rpm

%config_site	%{nil}
%ldflags	%{nil}
%_target_platform $TARGET_ARCH

%configure	\
  CFLAGS="%{cflags}" LDFLAGS="%{ldflags}" CONFIG_SITE=%{config_site} \
  %{sourcedir}/configure \
  --host=%{_host} --target=%{_target_platform} --prefix=%{native_prefix}
EOF

cp -f $base_macros $native_macros
cat >> $native_macros <<EOF
%config_site	%{_prefix}/share/native-config.site
#%program_suffix .exe
%program_suffix %{nil}

#%_arch		i686-cygwin
#%_vendor	pc
#%_os		cygwin

%_rpmfilename	%%{ARCH}-cygwin/%%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}-cygwin.rpm

%__find_provides	%{native_prefix}/lib/rpm/cygwin.prov
%__find_requires	%{native_prefix}/lib/rpm/cygwin.req

%ldflags	-L%{native_prefix}/lib %{cygwin_dll}
%configure	\
  CFLAGS="%{cflags}" LDFLAGS="%{ldflags}" CONFIG_SITE=%{config_site} \
  %{sourcedir}/configure --host=%{_host} --target=%{_target_platform} \
  --prefix=%{native_prefix} --program-suffix=%{program_suffix}
EOF

rm -f $native_rpm_dir/cygwin.prov
cat > $native_rpm_dir/cygwin.prov <<EOF
#!/bin/sh
while read f ; do
       f=\`echo $f | tr '[:upper:]' '[:lower:]'\`
       case \$f in
       *.dll)  basename \$f
               ;;
       esac
done | sort -u
EOF
chmod 755 $native_rpm_dir/cygwin.prov

rm -f $native_rpm_dir/cygwin.req
cat > $native_rpm_dir/cygwin.req <<EOF
#!/bin/sh

while read f ; do
       case \$f in
       *.dll|*.exe)    objdump -p $f | grep "DLL Name:" | cut -f3 -d" " | tr '[
upper:]' '[:lower:]'
                       ;;
       esac
done | sort -u

EOF
chmod 755 $native_rpm_dir/cygwin.req


set -x
type=rpm
rm -rf $NATIVE_ROOT/var/lib/rpm
mkdir -p $NATIVE_ROOT/var/lib/rpm
rpm --root $NATIVE_ROOT --initdb

topdir=$NATIVE_PREFIX/src/redhat
mkdir -p $topdir/{BUILD,RPMS/{$ta,$cta},SOURCES,SPECS,SRPMS}

##############
# end of setup
##############


PATH=$PREFIX/bin:$PATH

mkdir -p $PREFIX/src
cd $PREFIX/src
type=cross
for i in $cross_rpm; do
	if build $i; then
		true
	else
		echo "$i: rpm build failed"
		exit 1
	fi
done

# urg, bug in binutil's cross-make install
(
cd $PREFIX/$TARGET_ARCH/bin
ln -f ../../bin/$TARGET_ARCH-objdump objdump
ln -f ../../bin/$TARGET_ARCH-objcopy objcopy
)
# urg, bug in gcc's cross-make install
(
cd $PREFIX/$TARGET_ARCH/bin
ln -f ../../bin/$TARGET_ARCH-gcc cc
ln -f ../../bin/$TARGET_ARCH-gcc gcc
ln -f ../../bin/$TARGET_ARCH-c++ c++
ln -f ../../bin/$TARGET_ARCH-g++ g++
)

PATH=$PREFIX/$TARGET_ARCH/bin:$PATH

cd $PREFIX/src
type=native
for i in $native_rpm; do
	if build $i; then
		true
	else
		echo "$i: rpm build failed"
		exit 1
	fi
done

cd $NATIVE_PREFIX/src/redhat/BUILD

rm -f $distdir/$CYGWIN_DLL.gz
cd $distdir && cp -f $PREFIX/bin/$CYGWIN_DLL . && gzip -f $CYGWIN_DLL

rm -f $distdir/rpm2cpio.gz
cd $distdir && cp -f $PREFIX/bin/rpm2cpio . && gzip -f rpm2cpio


cat > $distdir/setup.sh <<EOF
#!/bin/bash
set -x
ROOT=/Cygnus
PREFIX=\$ROOT/usr
gunzip rpm2cpio.gz || exit 1
# currently, the cygwin1.dll's conflict.
# it's been reported one can't have both in PATH
dll=\$ROOT/net-485
mkdir -p \$dll
gzip -dc cygwin.dll.gz > \$dll/cygwin1.dll
here=\`pwd\`
cd \$ROOT/.. && (PATH=\$dll \$here/rpm2cpio \$here/rpm*.rpm ) | cpio -ivmd 
PATH=\$dll rpm --root=\$ROOT --initdb
cd \$here
for i in RPMS/$ta/*.rpm; do
	PATH=\$dll rpm -ivv --ignoreos --ignorearch --nodeps --force\
		  --dbpath \$ROOT/var/lib/rpm \
		  \$i
done
EOF

cat > $distdir/setup.bat <<EOF
bash setup.sh
if errorlevel 0 goto exit
@echo "setup.bat: can't find bash"
@echo "setup.bat: please install usertools from"
@echo "setup.bat: http://sourceware.cygnus.com/cygwin/"
:exit
EOF

cat > $distdir/lilypond.sh <<EOF
#!/bin/bash
ROOT=/Cygnus
PREFIX=\$ROOT/usr
# currently, the cygwin1.dll's conflict.
# it's been reported one can't have both in PATH
dll=\$ROOT/net-485
PATH=\$dll \$ROOT/bin/lilypond \$*
EOF

cat > $distdir/midi2ly.sh <<EOF
#!/bin/bash
ROOT=/Cygnus
PREFIX=\$ROOT/usr
# currently, the cygwin1.dll's conflict.
# it's been reported one can't have both in PATH
dll=\$ROOT/net-485
PATH=\$dll \$ROOT/bin/midi2ly \$*
EOF

cat > $distdir/lilypond.bat <<EOF
bash lilypond.sh %1 %2 %3 %4 %5 %6 %7 %8 %9
EOF

cat > $distdir/midi2ly.bat <<EOF
bash midi2ly.sh %1 %2 %3 %4 %5 %6 %7 %8 %9
EOF

distbase=`basename $distdir`
cd $distdir
rm -f RPMS
ln -s ../redhat/RPMS .

www=`dirname $distdir`
cd $www
for i in guile-1 rpm lilypond; do
	rpm=`find_path $i*.rpm $distbase/RPMS/$ta`
	dist_rpms="$dist_rpms $rpm"
done

rm -f $www/setup.zip
cd $www && zip setup.zip lily-w32 $distbase/* $dist_rpms
