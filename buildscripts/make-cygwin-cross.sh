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

TARGET_OS=cygwin
#TARGET_OS=mingw32

if [ $TARGET_OS = cygwin ]; then
	ROOT=/usr/src/cygwin-net-485
	TARGET_ARCH=i686
	TARGET_VENDOR=pc
else
	ROOT=/usr/src/mingw-net-485
	TARGET_ARCH=i386
	TARGET_VENDOR=pc
fi
TARGET_PLATFORM=$TARGET_ARCH-$TARGET_VENDOR-$TARGET_OS

ARCH=`uname -m`
OS=`uname -s | tr '[A-Z]' '[a-z]'`
VENDOR=gnu
PLATFORM=$ARCH-$VENDOR-$OS

#CROSS_TARGET_PLATFORM=$ARCH-x-$TARGET_PLATFORM
CROSS_TARGET_PLATFORM=$PLATFORM-x-$TARGET_PLATFORM
cto=$OS-x-cygwin
tp=i686-cygwin

PREFIX=$ROOT/usr
NATIVE_ROOT=/Cygnus
NATIVE_PREFIX=$NATIVE_ROOT/usr
CROSS_ROOT=$NATIVE_ROOT/$cto
CROSS_PREFIX=$CROSS_ROOT/usr

# urg
DEVEL=/home/fred
distdir=$DEVEL/WWW/lilypond/gnu-windows/lily-w32
#distdir=/tmp

CYGWIN_SOURCE=$DEVEL/sourceware.cygnus.com/pub/cygwin/private/cygwin-net-485
cygwin_dirs=`/bin/ls -d1 $CYGWIN_SOURCE/*`
cygwin_source_path=`echo $CYGWIN_SOURCE $cygwin_dirs | sed s'/\$/:/g'`
SOURCE_PATH=$DEVEL/usr/src/releases:$DEVEL/usr/src/redhat/SRPMS:$cygwin_source_path

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
gcc-2.95.2
bison
flex
"

# binutils installs ok, but forgets these two
binutils_after_rpm='
cd $CROSS_PREFIX/$TARGET_PLATFORM/bin \;
ln -f ../../bin/$TARGET_PLATFORM-objdump objdump \;
ln -f ../../bin/$TARGET_PLATFORM-objcopy objcopy \;
mv -f ld ld-in-path-breaks-libtool
'
# burp.  gcc doesn't install any of these
gcc_after_rpm='
cd $CROSS_PREFIX/$TARGET_PLATFORM/bin \;
ln -f ../../bin/$TARGET_PLATFORM-gcc cc \;
ln -f ../../bin/$TARGET_PLATFORM-gcc gcc \;
ln -f ../../bin/$TARGET_PLATFORM-c++ c++ \;
ln -f ../../bin/$TARGET_PLATFORM-g++ g++
'

guile_after_rpm='cp -f $NATIVE_PREFIX/bin/*guile-config $CROSS_PREFIX/$TARGET_PLATFORM/bin'

lilypond_version=@TOPLEVEL_VERSION@

native_rpm="
rx-1.5
zlib-1.1.3
db-2.7.7
guile-1.3.4
bash-2.03
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
		target_platform=$CROSS_TARGET_PLATFORM
		a=$cto
	else
		target_platform=$TARGET_PLATFORM
		a=$tp
	fi

	##rpm_package=`find_path $name*.rpm "$topdir/RPMS/$cto:$topdir/RPMS/$tp"`
	rpm_package=`find_path $package*.rpm "$topdir/RPMS/$cto:$topdir/RPMS/$tp"`
	if [ "$rpm_package" != "" ]; then
		echo "$rpm_package: package exists"
		#echo "$rpm_package: refreshing installation only"
		echo "$rpm_package: just updating rpm database"
		rpm -ivv --justdb --ignoreos --ignorearch --nodeps --force\
			--dbpath $NATIVE_ROOT/var/lib/rpm \
			$topdir/RPMS/$a/$name*.rpm
		exit 0
	fi

	cd $topdir
	found=`find_path $package*src.rpm`
	if [ "$found" != "" ]; then
		rpm --root $NATIVE_ROOT -ivv $found || exit 1
		rpm --target=$target_platform -ba SPECS/$name*.spec || exit 1
	else
		tarball=`/bin/ls -d1 SOURCES/$package*tar.gz 2>/dev/null | head -1`
		if [ "$name.spec" != "" ] && [ "$tarball" != "" ]; then
			rpm --target=$target_platform -ba SPECS/$name.spec || exit 1
		else
			found=`find_path $package*tar.gz`
			if [ "$found" != "" ]; then
				rpm --target=$target_platform -ta $found || exit 1
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
	#urg
        name_after_rpm="`expand $name _after_rpm`"
	`eval $name_after_rpm` || exit 1
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

	## is this still necessary?
	mkdir -p include lib
	mkdir -p $TARGET_PLATFORM
	cd $TARGET_PLATFORM && ln -s ../include .

	mkdir -p $CROSS_PREFIX/$TARGET_PLATFORM
	cd $CROSS_PREFIX/$TARGET_PLATFORM
	ln -s $NATIVE_PREFIX/{include,lib} .
	#ln -s $NATIVE_PREFIX/{include,lib} .
	#ln -s $NATIVE_PREFIX/include .
	#ln -s $CROSS_PREFIX/bin .
	
	cd $ROOT
	found=`find_path $cygwin_binary`
	if [ "$found" = "" ]; then
		echo "$cygwin_binary: no such tarball"
		exit 1
	fi
	tar xzf $found
	# urg, bug in gcc's cross-make configuration
	mkdir -p $PREFIX/lib/gcc-lib/$TARGET_PLATFORM/2.95.2
	mkdir -p $CROSS_PREFIX/lib/gcc-lib/$TARGET_PLATFORM/2.95.2
else
	echo "$PREFIX: already exists"
	echo "$cygwin_binary: skipping"
fi

# mingw
if [ ! -d $PREFIX/$TARGET_PLATFORM ]; then
	cd $PREFIX
	found=`find_path $mingw_binary`
	if [ "$found" = "" ]; then
		echo "$mingw_binary: no such tarball"
		exit 1
	fi
	tar xzf $found

	mkdir -p $PREFIX/lib/gcc-lib/$TARGET_PLATFORM/2.95.2
	rm -f include
	mkdir -p $PREFIX/i386-mingw32/include
	ln -s $PREFIX/i386-mingw32 $TARGET_PLATFORM
	ln -s $PREFIX/$TARGET_PLATFORM/include .
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
ac_cv_c_bigendian=no
ac_cv_sizeof_char_p=4
ac_cv_sizeof_int=4
ac_cv_sizeof_long=4
ac_cv_sizeof_float=4
ac_cv_sizeof_double=8
ac_cv_sys_restartable_syscalls=no
ac_cv_sprintf_count=yes
ac_cv_spinlocks=no
db_cv_sprintf_count=yes
db_cv_spinlocks=no
ac_cv_func_getpgrp_void=yes
ac_cv_func_setvbuf_reversed=no
ac_cv_lib_intl=no
# urg, lots of stuff goes wrong when configuring bash??
ac_cv_exeext=.exe
ac_cv_header_libintl_h=no
ac_cv_header_termcap_h=no
ac_cv_func_mkfifo=yes
bash_cv_dup2_broken=no
bash_cv_opendir_not_robust=no
bash_cv_pgrp_pipe=no
bash_cv_printf_declared=yes
bash_cv_sbrk_declared=yes
bash_cv_signal_vintage=posix
bash_cv_speed_t_in_sys_types=no
bash_cv_struct_timeval=yes
bash_cv_struct_winsize_header=ioctl_h
bash_cv_sys_errlist=yes
bash_cv_sys_named_pipes=missing
bash_cv_func_strcoll_broken=no
bash_cv_must_reinstall_sighandlers=no
bash_cv_getcwd_calls_popen=no
bash_cv_func_sigsetjmp=missing
bash_cv_job_control_missing=present
bash_cv_sys_restartable_syscalls=no
bash_cv_getenv_redef=yes
bash_cv_sys_siglist=no
bash_cv_type_rlimit=long
bash_cv_ulimit_maxfds=no
bash_cv_decl_under_sys_siglist=no
bash_cv_under_sys_siglist=no
EOF

#URG, stupid RPM
rpm_ct=$ARCH-$TARGET_OS
cross_rpm_dir=$CROSS_PREFIX/lib/rpm/$rpm_ct
cross_rpm_link=/usr/lib/rpm/$rpm_ct
mkdir -p $cross_rpm_dir
rm -f $cross_rpm_link
ln -s $cross_rpm_dir $cross_rpm_link

native_rpm_dir=$NATIVE_PREFIX/lib/rpm/$tp
native_rpm_link=/usr/lib/rpm/$tp
mkdir -p $native_rpm_dir
rm -f $native_rpm_link
ln -s $native_rpm_dir $native_rpm_link

cross_macros=$cross_rpm_dir/macros
native_macros=$native_rpm_dir/macros

base_macros=$NATIVE_PREFIX/lib/rpm/base-macros
rm -f $base_macros
cat > $base_macros <<EOF
%native_prefix	/Cygnus/usr

%_topdir	%{native_prefix}/src/redhat
%_sourcedir	%{native_prefix}/src/redhat/SOURCES
%_builddir	%{native_prefix}/src/redhat/BUILD
%_srcrpmdir	%{native_prefix}/src/redhat/SRPMS
%_rpmdir	%{native_prefix}/src/redhat/RPMS

#%DocDir		%{native_prefix}/doc
%DocDir		/Cygnus/usr/doc
%_docdir	/Cygnus/usr/doc
%_defaultdocdir /Cygnus/usr/doc

%cygwin_dll	%{native_prefix}/bin/cygwin1.dll

%__install	/bin/install

%cflags		%{optflags}
%cppflags	%{nil}

%sourcedir	.
EOF

cp -f $base_macros $cross_macros
cat >> $cross_macros <<EOF

%cross_prefix	/Cygnus/$cto/usr
%root		/Cygnus/$cto
%_rootbindir    /Cygnus/$cto/bin
#_usr           /Cygnus/$cto/usr
%_prefix	/Cygnus/$cto/usr
%prefix		/Cygnus/$cto/usr

%_rpmfilename	$cto/%%{NAME}-%%{VERSION}-%%{RELEASE}.$cto.rpm

%config_site	%{nil}

%cppflags	%{nil}
%cflags		%{nil}
%ldflags	%{nil}
%_target_platform $TARGET_PLATFORM

#% configure	\
#  %{?__libtoolize:[ -f configure.in ] \&\& %{__libtoolize} --copy --force}; \
%configure	\
  CPPFLAGS="%{cppflags}" CFLAGS="%{cflags}" LDFLAGS="%{ldflags}" CONFIG_SITE=%{config_site} \
  %{sourcedir}/configure \
  --host=%{_host} --target=%{_target_platform} --prefix=%{cross_prefix} -v
EOF

cp -f $base_macros $native_macros
cat >> $native_macros <<EOF

%root		/Cygnus
%_rootbindir    /Cygnus/bin
#_usr           /Cygnus/usr
%_prefix	/Cygnus/usr
%prefix		/Cygnus/usr

%config_site	%{_prefix}/share/native-config.site
# won't work: scripts and stuff are .exe too
#%program_suffix .exe
%program_suffix %{nil}

#%_arch		i686-cygwin
#%_vendor	pc
#%_os		cygwin

%_rpmfilename	%%{ARCH}-$TARGET_OS/%%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}-$TARGET_OS.rpm

%__find_provides	%{native_prefix}/lib/rpm/cygwin.prov
%__find_requires	%{native_prefix}/lib/rpm/cygwin.req
%__fix_suffixes		%{native_prefix}/bin/fix-suffixes
%fix_suffixes	%{__fix_suffixes} \$RPM_BUILD_ROOT%{_rootbindir}/* \$RPM_BUILD_ROOT%{native_prefix}/bin/*

%cppflags	-I%{native_prefix}/include
%cflags		%{optflags} %{cppflags}
%ldflags	-L%{native_prefix}/lib %{cygwin_dll}

#%configure	\
#  %{?__libtoolize:[ -f configure.in ] \&\& %{__libtoolize} --copy --force}; \

%configure	\
  CPPFLAGS="%{cppflags}" CFLAGS="%{cflags}" LDFLAGS="%{ldflags}" CONFIG_SITE=%{config_site} \
  %{sourcedir}/configure --host=%{_host} --target=%{_target_platform} \
  --prefix=%{native_prefix} --program-prefix=
#  --prefix=%{native_prefix} --program-suffix=%{program_suffix}
EOF

rm -f $NATIVE_PREFIX/lib/rpm/cygwin.prov
cat > $NATIVE_PREFIX/lib/rpm/cygwin.prov <<EOF
#!/bin/sh
while read f ; do
       f=\`echo \$f | tr '[:upper:]' '[:lower:]'\`
       case \$f in
       *.dll)  basename \$f
               ;;
       esac
done | sort -u
EOF
chmod 755 $NATIVE_PREFIX/lib/rpm/cygwin.prov

rm -f $NATIVE_PREFIX/lib/rpm/cygwin.req
cat > $NATIVE_PREFIX/lib/rpm/cygwin.req <<EOF
#!/bin/sh

while read f ; do
       case \$f in
       *.dll|*.exe)    objdump -p \$f | grep "DLL Name:" | cut -f3 -d" " | tr '[:upper:]' '[:lower:]'
                       ;;
       esac
done | sort -u

EOF
chmod 755 $NATIVE_PREFIX/lib/rpm/cygwin.req

rm -f $NATIVE_PREFIX/bin/fix-suffixes
cat > $NATIVE_PREFIX/bin/fix-suffixes <<EOF
#!/bin/sh
## duh, rename executables,
## for people that use a dumb shell instead of bash


if [ \$# -le 0 ]; then
	echo "Usage: fix-suffixes [FILE]..."
	exit 2
fi

fix_extension ()
{
        path=\$1
        ext=\$2
        expr="\$3"
	dir=\`dirname \$path\`
        file=\`basename \$path\`
        base=\`basename \$file \$ext\`
        if [ \$base\$ext != \$file ]; then
                type="\`file \$path\`"
                if expr "\$type" : "\$expr"; then
                        mv -f \$path \$dir/\$base\$ext
                fi
        fi
}

for i in \`/bin/ls -d1 \$*\`; do
	fix_extension \$i .exe '.*Windows.*\(executable\).*'
	fix_extension \$i .py '.*\(python\).*'
done

EOF
chmod 755 $NATIVE_PREFIX/bin/fix-suffixes
set -x
type=rpm
rm -rf $NATIVE_ROOT/var/lib/rpm
mkdir -p $NATIVE_ROOT/var/lib/rpm
rpm --root $NATIVE_ROOT --initdb

topdir=$NATIVE_PREFIX/src/redhat
mkdir -p $topdir/{BUILD,RPMS/{$tp,$cto},SOURCES,SPECS,SRPMS}

##############
# end of setup
##############

OLDPATH=$PATH
# building x-gcc requires to have x-ar in path
# x-gcc will find $prefix/x/bin/as by itself
PATH=$CROSS_PREFIX/bin:$PATH

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

PATH=$CROSS_PREFIX/$TARGET_PLATFORM/bin:$PATH

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

##############
# Distribution
##############

PATH=$OLDPATH

mkdir -p $distdir

cat > $distdir/setup.sh <<EOF
#!/bin/bash
set -x
ROOT=/Cygnus
PREFIX=\$ROOT/usr
# gunzip rpm.gz || exit 1
#dll=\$ROOT/net-485
#mkdir -p \$dll
#gzip -dc cygwin.dll.gz > \$dll/cygwin1.dll
## this won't work: must be done outside of cygwin
#old_dll=\`which cygwin1.dll\`
#mv -f \$old_dll \$old_dll.orig\$\$
#gunzip cygwin1.dll.gz
#cp -f cygwin1.dll \$old_dll
echo > rpmrc
# mkdir -p \$ROOT/var/lib
# mkdir -p \$ROOT/bin
rem # touch \$ROOT/bin/rpm \$ROOT/bin/rpm.exe
echo > \$ROOT/bin/rpm; echo > \$ROOT/bin/rpm.exe
./rpm --root \$ROOT --rcfile rpmrc --initdb
./rpm --root \$ROOT --rcfile rpmrc --nodeps --ignorearch --ignoreos -Uhv \\
	RPMS/$tp/rpm-*.$tp.rpm \\
	RPMS/$tp/*.$tp.rpm
done
EOF

cat > $distdir/setup.bat <<EOF
del dll olddll
rem # old_dll=\`which cygwin1.dll\`
rem # urg, should dist bash too, then we don't need usertools!
rem # but this gets ugly, we could really use:
rem # 
rem #     cp, cygpath, gunzip, mv, mkdir, touch
rem #  
rem # ie, fileutils, gzip, (cygpath?)
rem #  
rem #	cp -f \`which bash\` /bin;
rem #   mkdir -p /bin;
rem #	cp -f bash /bin/bash.exe;
rem #	cp -f /bin/bash /bin/sh.exe;
mkdir \\bin
copy bash \\bin\\bash.exe
copy bash \\bin\\sh.exe
mkdir \\Cygnus
mkdir \\Cygnus\\bin
mkdir \\Cygnus\\var
mkdir \\Cygnus\\var\\lib
bash -c '
	dll=\`type -p cygwin1.dll\`;
	wdll=\`./cygpath -w \$dll\`;
	wdll=\${wdll:-\\Cygnus\\bin};
	echo cygwin1.dll \$wdll > newdll; echo \$wdll \$wdll.orig\$\$ > olddll'
if not errorlevel 0 goto nobash
rem # mv -f \$old_dll \$old_dll.orig\$\$
rem # gunzip cygwin1.dll.gz
rem # cp -f cygwin1.dll \$old_dll
copy < olddll 
copy < newdll 
bash setup.sh
if not errorlevel 0 goto nobash
attrib +s \\Cygnus\\usr\\share\\lilypond\\cmtfm
goto :exit
:nobash
@echo "setup.bat: can't find bash"
rem # @echo "setup.bat: please install usertools from"
rem # @echo "setup.bat: http://sourceware.cygnus.com/cygwin/"
:exit
EOF

cat > $distdir/lilypond.sh <<EOF
#!/bin/bash
ROOT=/Cygnus
PREFIX=\$ROOT/usr
\$ROOT/bin/lilypond \$*
EOF

cat > $distdir/midi2ly.sh <<EOF
#!/bin/bash
ROOT=/Cygnus
PREFIX=\$ROOT/usr
\$ROOT/bin/midi2ly \$*
EOF

cat > $distdir/lilypond.bat <<EOF
bash lilypond.sh %1 %2 %3 %4 %5 %6 %7 %8 %9
EOF

cat > $distdir/midi2ly.bat <<EOF
bash midi2ly.sh %1 %2 %3 %4 %5 %6 %7 %8 %9
EOF

cd $distdir
rm -f $CYGWIN_DLL.gz
cp -f $PREFIX/bin/$CYGWIN_DLL .
rm -f rpm.gz
cp -f `/bin/ls -d1 $ROOT/bin/rpm* |head -1` rpm

rm -f bash.gz
cp -f `/bin/ls -d1 $ROOT/bin/bash* |head -1` bash

rm -f cygpath.gz
cp -f `/bin/ls -d1 $PREFIX/bin/cygpath* |head -1` cygpath

distbase=`basename $distdir`
rm -f RPMS
ln -s ../redhat/RPMS .

www=`dirname $distdir`
cd $www
for i in bash-2 guile-1 rpm-3 lilypond-$lilypond_version; do
	rpm=`find_path $i*.rpm $distbase/RPMS/$tp`
	dist_rpms="$dist_rpms $rpm"
done

rm -f $www/setup*.zip
cd $www && zip setup-lilypond-$lilypond_version.zip lily-w32 $distbase/* $dist_rpms

# make small zip dist available
#
zipdir=$www/zip
mkdir -p $zipdir

rm -f $zipdir/$CYGWIN_DLL.zip
cd $ROOT/bin && zip $zipdir/$CYGWIN_DLL.zip $CYGWIN_DLL

for i in bash-2 guile-1 rpm-3 lilypond-$lilypond_version; do
	found=`find_path $i*.rpm $distdir/RPMS/$tp`
	if [ "$found" = "" ]; then
		echo "$i: no such .rpm"
	else
		base=`basename $found`
		package=`echo $base | sed "s/\.$tp.rpm//"`
		dir=/tmp/$i-rpm2zip
		rm -rf $dir
		mkdir -p $dir
		cd $dir || exit 1
		rpm2cpio $found | cpio -id
		zip -ry $zipdir/$package.zip .
		rm -rf $dir
	fi
done

cd $distdir
gzip -f $CYGWIN_DLL
gzip -f rpm
gzip -f bash
gzip -f cygpath

