%define name lilypond
%define version 1.5.47
%define release 1mdk

Name: %{name}
Summary: A program for printing sheet music.
Version: %{version}
Release: %{release}
Source0: ftp.cs.uu.nl:/pub/GNU/LilyPond/development/lilypond-%{version}.tar.gz
## Patch0: lilypond-GNUmakefile.in.patch.bz2
License: GPL
Group: Publishing
URL: http://www.lilypond.org/
BuildRoot: %{_tmppath}/%{name}-buildroot
BuildRequires: guile guile-devel
Requires: tetex

%description
LilyPond is a music typesetter.  It produces beautiful sheet music using a
high level description file as input.  Lilypond is part of the GNU project.
 
LilyPond is split into two packages.  The package "lilypond" provides the
core package, containing the utilities for converting the music source
(.ly) files into printable output.  The package "lilypond-extras" provides
the full documentation, example .ly files for various features and the
Mutopia project files (musical equivalent of the Gutenberg project - see
http://www.mutopiaproject.org for details).
 
If you are new to lilypond, you will almost certainly want to install the
"lilypond-extras" package in addition to the "lilypond" package.
 
You may also wish to investigate the "denemo" package, which provides a
graphical front end to lilypond.

See the file README.first for more information.


%package extras
Summary: LilyPond documentation, examples and Mutopia files.
Group: Publishing
Requires: lilypond

%description extras
The documentation of LilyPond, both in HTML and PostScript, along with
example input files and the files from the Mutopia project.

%prep
%setup -a 1
## % patch0 -b .orig

%build
# DO NOT use % { configure } , it hardcodes all paths, runs libtool,
# so we can't do make prefix=/tmp/ install.
# -- not sure what this comment means; it's a relic from the PPC spec file -- mbrown@fensystems.co.uk
./configure --disable-checking --disable-debugging --enable-printing --prefix=%{_prefix} --disable-optimise --enable-shared
make
make local-web

%install

# Create installation root folders
rm -rf $RPM_BUILD_ROOT
RPM_DOC_ROOT=$RPM_BUILD_ROOT%{_docdir}/%{name}-%{version}
rm -rf $RPM_DOC_ROOT
mkdir -p $RPM_DOC_ROOT

make prefix="$RPM_BUILD_ROOT%{_prefix}" install
%{find_lang} %{name}

# Move info and man files into correct locations
mv $RPM_BUILD_ROOT/usr/info $RPM_BUILD_ROOT%{_infodir}
mv $RPM_BUILD_ROOT/usr/man $RPM_BUILD_ROOT%{_mandir}

# Move TeX font files into system TeX tree locations
mkdir -p $RPM_BUILD_ROOT%{_datadir}/texmf/fonts/{afm,source,tfm}
mv $RPM_BUILD_ROOT%{_datadir}/lilypond/afm $RPM_BUILD_ROOT%{_datadir}/texmf/fonts/afm/lilypond
mv $RPM_BUILD_ROOT%{_datadir}/lilypond/mf $RPM_BUILD_ROOT%{_datadir}/texmf/fonts/source/lilypond
mv $RPM_BUILD_ROOT%{_datadir}/lilypond/tfm $RPM_BUILD_ROOT%{_datadir}/texmf/fonts/tfm/lilypond
mkdir -p $RPM_BUILD_ROOT%{_datadir}/texmf/tex
mv $RPM_BUILD_ROOT%{_datadir}/lilypond/tex $RPM_BUILD_ROOT%{_datadir}/texmf/tex/lilypond

# Copy emacs mode files into %{_datadir}/lilypond/emacs
mkdir -p $RPM_BUILD_ROOT%{_datadir}/lilypond/emacs
cp *.el $RPM_BUILD_ROOT%{_datadir}/lilypond/emacs/

# Create documentation tree in %{_docdir}
mkdir -p $RPM_DOC_ROOT/HTML
tar -C $RPM_DOC_ROOT/HTML -xzf out/web.tar.gz
ln -s HTML/input $RPM_DOC_ROOT/Examples
ln -s HTML/input/test $RPM_DOC_ROOT/Tricks
ln -s HTML/mutopia $RPM_DOC_ROOT/Mutopia
mkdir -p $RPM_DOC_ROOT/Printable
ln -s ../HTML/Documentation/user/out-www/lilypond.ps.gz $RPM_DOC_ROOT/Printable/Manual.ps.gz
ln -s ../HTML/Documentation/user/out-www/glossary.ps.gz $RPM_DOC_ROOT/Printable/Glossary.ps.gz
cp AUTHORS.txt CHANGES COPYING DEDICATION FAQ.txt NEWS README.txt VERSION README.mandrake $RPM_DOC_ROOT/README.first

%post
touch /tmp/.lilypond-install
rm `find /var/lib/texmf -name 'feta*pk -print' -or -name 'feta*tfm -print'` /tmp/.lilypond-install
/usr/bin/mktexlsr
%_install_info lilypond.info
%_install_info lilypond-internals.info

echo 'Please logout first before using LilyPond.'


%preun
%_remove_install_info lilypond.info
%_remove_install_info lilypond-internals.info

%postun
/usr/bin/mktexlsr


%files -f %{name}.lang
%defattr(-,root,root,0755)
%{_bindir}/*
%{_datadir}/lilypond/
%{_datadir}/texmf/fonts/afm/lilypond
%{_datadir}/texmf/fonts/source/lilypond
%{_datadir}/texmf/fonts/tfm/lilypond
%{_datadir}/texmf/tex/lilypond

%doc %{_infodir}/*
%doc %{_mandir}/man1/*
%doc %{_docdir}/%{name}-%{version}/AUTHORS.txt
%doc %{_docdir}/%{name}-%{version}/CHANGES
%doc %{_docdir}/%{name}-%{version}/COPYING
%doc %{_docdir}/%{name}-%{version}/DEDICATION
%doc %{_docdir}/%{name}-%{version}/FAQ.txt
%doc %{_docdir}/%{name}-%{version}/NEWS
%doc %{_docdir}/%{name}-%{version}/README.txt
%doc %{_docdir}/%{name}-%{version}/VERSION
%doc %{_docdir}/%{name}-%{version}/README.first

%files extras
%defattr(-,root,root,0755)
%doc %{_docdir}/%{name}-%{version}/HTML
%doc %{_docdir}/%{name}-%{version}/Mutopia
%doc %{_docdir}/%{name}-%{version}/Printable
%doc %{_docdir}/%{name}-%{version}/Tricks
%doc %{_docdir}/%{name}-%{version}/Examples


%changelog
* Fri Jun 01 2001 Lenny Cartier <lenny@mandrakesoft.com> 1.4.2-1mdk
- updated by Michael Brown <mbrown@linux-mandrake.com> :
	- Upgraded to 1.4.2
	- Changed URL
	- Removed /etc/profile.d scripts: all TeX bits are now located properly
	- Rearranged documentation
	- Tidied spec file
	- Updated README.first

* Mon Feb 26 2001 Lenny Cartier <lenny@mandrakesoft.com> 1.3.129-1mdk
- added in contribs by Michael Brown <mbrown@fensystems.co.uk> :
	- Removed info files completely (couldn't get info reader to read them)
	- TODO: Get this to work at some point (in the meantime, use documentation in lilypond-extras)

* Sat Feb 17 2001 Michael Brown <mbrown@fensystems.co.uk>
- Moved TeX font folders into the system TeX tree (this ensures that the /var/lib/texmf font cache is used instead of current folder)
- Added mktexlsr to post-install and post-uninstall scripts
- Patched scripts in /etc/profile.d to reflect changes to font locations
- Added QuickStart guide and Points to note sections to README.first (well worth reading)
- Fixed install-info and uninstall-info sections to use Mandrake's RPM macros :-)

* Tue Feb 13 2001 Michael Brown <mbrown@fensystems.co.uk>
- First Mandrake package
- spec file heavily adapted from PowerPC contribs.
- Note HTML documentation may contain duff links and may be incomplete.  Lots of warnings appear when building HTML docs.
- PostScript documentation is duplicated (how do you get an rpm package to contain a symlink?)
- Created README.first file
