Name: stepmake
Version: 0.1.49
Release: 1
Copyright: GPL
Group: Development
Source0: pcnov095.win.tue.nl:/pub/lilypond/development/stepmake-0.1.49.tar.gz
Summary: generic make package
Packager: janneke@gnu.org (Jan Nieuwenhuizen)
Buildroot: /tmp/stepmake-install

%description 


%prep
%setup
%build
./configure --prefix=/usr
make all
%install
rm -rf $RPM_BUILD_ROOT
make prefix="$RPM_BUILD_ROOT/usr" install
%files
# urg
/usr/bin/make-patch
%post

