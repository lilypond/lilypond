#
# spec file for package lilypond (Version 1.3.149)
# based on 
#    spec file for package lilypond (Version 1.2.17)
#    Copyright  (c)  2000  SuSE GmbH  Nuernberg, Germany.
#
# Please send bug reports to schlemme@mathe.tu-freiberg.de

%define info yes

# neededforbuild  guile tcsh tetex te_latex te_mpost libpng python gpp libgpp gettext autoconf netpbm libnetpb gs_serv gs_lib gs_fonts guile
# usedforbuild    aaa_base aaa_dir autoconf automake base bash bindutil binutils bison bzip compress cpio cracklib devs diff ext2fs file fileutil find flex gawk gcc gdbm gettext gpm gpp gppshare groff gs_fonts gs_lib gs_serv guile gzip kbd less libc libgpp libnetpb libpng libtool libz lx_suse make mktemp modules ncurses net_tool netcfg netpbm nkita nkitb nssv1 pam patch perl pgp ps python rcs rpm sendmail sh_utils shadow shlibs strace syslogd sysvinit tcsh te_ams te_latex te_mpost tetex texinfo textutil timezone unzip util vim xdevel xf86 xshared guile


Distribution: SuSE Linux 7.0 (i386)
Name: lilypond
Version: 1.5.48
Release: 2
Copyright:    GPL
Group: Applications/Publishing
Source0: ftp.cs.uu.nl:/pub/GNU/LilyPond/development/lilypond-1.5.48.tar.gz
# music notation software for.. ?
Summary: A program for printing sheet music.
URL: http://www.lilypond.org/
# rpm: 4.0: broken for -ta builds: rpm doesn't look in tarball for xpm
# Icon: lilypond-icon.xpm
BuildRoot: /tmp/lilypond-install
# add lots of Buildreq: tetex-kpath, te_mpost, bison
# better prereqs: tetex-latex, python, (mpost?) etc.
Prereq: tetex python

# use keywords: music notation software
%description
LilyPond is a music typesetter.  It produces beautiful
sheet music using a high level description file as input.  LilyPond is
part of the GNU Project.

Authors:
--------
    Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
    Alexandre Oliva <oliva@dcc.unicamp.br>
    Mats Bengtsson <matsb@s3.kth.se>
    Eric Bullinger <eric@aut.ee.ethz.ch>
    Jan Arne Fagertun <Jan.A.Fagertun@energy.sintef.no>
    Anthony Fok <foka@debian.org>
    Neil Jerram <nj104@cus.cam.ac.uk>
    Donald Ervin Knuth
    Werner Lemberg <wl@gnu.org>
    David R. Linn <drl@vuse.vanderbilt.edu>
    François Pinard <pinard@iro.umontreal.ca>
    Jeffrey B. Reed <daboys@bga.com>
    Shay Rojanski
    Tom Cato Amundsen <tca@gnu.org>
    Laura Conrad <lconrad@world.std.com>
    James Hammons <jlhamm@pacificnet.net>
    Bjoern Jacke <bjoern.jacke@gmx.de>
    Michael Krause <m.krause@tu-harburg.de>
    David R. Linn <drl@vuse.vanderbilt.edu>
    Adrian Mariano
    Stephen Peters <portnoy@ai.mit.edu>
    Glen Prideaux <glenprideaux@iname.com>
    Roy R. Rankin <Roy.Rankin@alcatel.com.au>
    Juergen Reuter <reuterj@ira.uka.de>
    August S.Sigov <august@infran.ru>
    Rune Zedeler <rune@zedeler.dk>

SuSE series: ap

%package doc
Summary: Prebuilt website containing all LilyPond documentation.
Group: Applications/Publishing
# BuildArchitectures: noarch

%description doc

The documentation of LilyPond, both in HTML and PostScript.

%define INSTALL install -m755 -s
%define INSTALL_DIR install -d -m755
%define INSTALL_DATA install -m644
%prep
%setup

%build

#

# DO NOT use % { configure } , it hardcodes all paths, runs libtool,
# so we can't do make prefix=/tmp/ install.

# In fact, do not take out the spaces between % and { , because RPM will gladly
# do a substitution anyway.

CFLAGS="$RPM_OPT_FLAGS" ./configure --disable-checking --disable-debugging --enable-printing --prefix=%{_prefix} --enable-optimise --enable-shared

make LDFLAGS=-s "CFLAGS=$RPM_OPT_FLAGS" all

# urg
# %build doc
# line 42: second %build
# ok, now make sure that lilypond package will succeed,
# even if documentation fails to build

make -C Documentation  || true
make web-doc top-web || true

%install



rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/tmp/lilypond-rpm-doc

## this is an ugly hack
mkdir -p scripts/share/lilypond/tex
cp tex/titledefs.tex scripts/share/lilypond/tex
## end of hack


strip lily/out/lilypond 
make prefix="$RPM_BUILD_ROOT%{_prefix}" install

%{INSTALL_DIR} $RPM_BUILD_ROOT/usr/share/texmf/fonts/source/public/lilypond
(cd $RPM_BUILD_ROOT/usr/share/lilypond \
    && mv mf/* $RPM_BUILD_ROOT/usr/share/texmf/fonts/source/public/lilypond \
    && rm -fr mf \
    && ln -s ../texmf/fonts/source/public/lilypond mf)
%{INSTALL_DIR} $RPM_BUILD_ROOT/usr/share/texmf/fonts/afm/lilypond
(cd $RPM_BUILD_ROOT/usr/share/lilypond \
    && mv afm/* $RPM_BUILD_ROOT/usr/share/texmf/fonts/afm/lilypond \
    && rm -fr afm \
    && ln -s ../texmf/fonts/afm/lilypond afm)
%{INSTALL_DIR} $RPM_BUILD_ROOT/usr/share/texmf/tex/lilypond
(cd $RPM_BUILD_ROOT/usr/share/lilypond \
    && mv tex/* $RPM_BUILD_ROOT/usr/share/texmf/tex/lilypond \
    && rm -fr tex \
    && ln -s ../texmf/tex/lilypond tex)
%{INSTALL_DIR} $RPM_BUILD_ROOT/usr/share/texmf/lilypond/ps
(cd $RPM_BUILD_ROOT/usr/share/lilypond \
    && mv ps/* $RPM_BUILD_ROOT/usr/share/texmf/lilypond/ps \
    && rm -fr ps \
    && ln -s ../texmf/lilypond/ps ps)


%if info=="yes"
gzip -9fn $RPM_BUILD_ROOT%{_prefix}/info/* || true
%endif



gzip -9fn $RPM_BUILD_ROOT%{_prefix}/man/man1/* || true




mkdir -p $RPM_BUILD_ROOT%{_prefix}/../etc/profile.d
cp buildscripts/out/lilypond-profile $RPM_BUILD_ROOT%{_prefix}/../etc/profile.d/lilypond.sh
cp buildscripts/out/lilypond-login $RPM_BUILD_ROOT%{_prefix}/../etc/profile.d/lilypond.csh

# urg
#%install doc
#line 63: second %install
# again, make sure that main package installs even if doco fails
mkdir -p web/out
tar -C web -xzf out/web.tar.gz || true

%ifos cygwin
# urg, this symlink doesn't come through on cygwin
# this is the way symlinks work over there, let's fake one
rm -f $RPM_BUILD_ROOT%{_prefix}/share/lilypond/cmtfm
echo '!<symlink>c:\\texmf\\fonts\\tfm\\public\\cm' > $RPM_BUILD_ROOT%{_prefix}/share/lilypond/cmtfm
%{fix_suffixes}
%endif

%{?suse_check}


%pre
if [ -d usr/share/lilypond/ps ]; then
  mv usr/share/lilypond/ps usr/share/lilypond/ps.old
  echo "please, remove /usr/share/lilypond/ps.old manually."
fi

%post

touch /tmp/.lilypond-install
rm `find /var/lib/texmf -name 'feta*pk -print' -or -name 'feta*tfm -print'` /tmp/.lilypond-install
%if info=="yes"
/usr/bin/install-info %{_prefix}/info/lilypond.info.gz %{_prefix}/info/dir || true
%endif
mkdir -p var/adm/SuSEconfig
touch var/adm/SuSEconfig/run-texhash


%preun
%if info=="yes"
if [ $1 = 0 ]; then
    /usr/bin/install-info --delete %{_prefix}/info/lilypond.info.gz %{_prefix}/info/dir || true
fi
%endif

%postun
mkdir -p var/adm/SuSEconfig
touch var/adm/SuSEconfig/run-texhash



%files
%defattr(-, root, root)
%doc CHANGES COPYING DEDICATION INSTALL.txt NEWS
%doc README.txt FAQ.txt AUTHORS.txt VERSION ROADMAP
%doc buildscripts/
%doc scripts/
# hairy to hook it in (possibly non-existing) emacs
%doc lilypond-mode.el
%doc lilypond-font-lock.el

%config /etc/profile.d/*

%ifnos cygwin
%{_prefix}/bin/abc2ly
%{_prefix}/bin/as2text
%{_prefix}/bin/convert-ly
%{_prefix}/bin/etf2ly
%{_prefix}/bin/lilypond
%{_prefix}/bin/ly2dvi
%{_prefix}/bin/midi2ly
%{_prefix}/bin/lilypond-book
%{_prefix}/bin/mup2ly
%{_prefix}/bin/musedata2ly
%{_prefix}/bin/pmx2ly
%else
%{_prefix}/bin
%endif

%if info=="yes"
%{_prefix}/info/lilypond.info.gz
%{_prefix}/info/lilypond-internals.info.gz
%endif

%{_prefix}/man/man1/abc2ly.1.gz
%{_prefix}/man/man1/as2text.1.gz
%{_prefix}/man/man1/convert-ly.1.gz
%{_prefix}/man/man1/etf2ly.1.gz
%{_prefix}/man/man1/lilypond.1.gz
%{_prefix}/man/man1/ly2dvi.1.gz
%{_prefix}/man/man1/midi2ly.1.gz
%{_prefix}/man/man1/lilypond-book.1.gz
%{_prefix}/man/man1/musedata2ly.1.gz
%{_prefix}/man/man1/mup2ly.1.gz
%{_prefix}/man/man1/pmx2ly.1.gz

%{_prefix}/share/lilypond/
%{_prefix}/share/texmf/
%{_prefix}/share/locale/*/LC_MESSAGES/lilypond.mo
# urg?
#%{_prefix}/../etc/profile.d/lilypond.*

%files doc
# this gets too messy...
%doc mutopia/
%doc input/
%doc web/
# verbatim include of input: list the directory without issuing a %dir 

%changelog -n lilypond
* Sun Apr 22 2001 - schlemme@mathe.tu-freiberg.de
- update: 1.3.149
* Mon Apr 10 2000 - bk@suse.de
- added suse update config macro
* Thu Mar 16 2000 - kukuk@suse.de
- Use gs_serv, not gs_both (doesn't exist on all platforms)
* Wed Mar 01 2000 - uli@suse.de
- moved man pages to /usr/share
* Tue Dec 21 1999 - ke@suse.de
- add documentation (#271).
* Mon Dec 13 1999 - ke@suse.de
- update: 1.2.17.
- compiler fix (thanks to schwab@suse.de).
- #271.
* Mon Oct 25 1999 - ke@suse.de
- update: 1.2.16.
* Mon Sep 13 1999 - bs@suse.de
- ran old prepare_spec on spec file to switch to new prepare_spec.
* Mon Sep 06 1999 - ro@suse.de
- update to 1.2.6 to make it compile with new guile
- various fixes and one hack to make this compile at all
* Sun Aug 22 1999 - ke@suse.de
- provide /etc/profile.d scripts.
- make lily.ps available (thanks to Ulrich Windl).
* Tue Aug 17 1999 - ke@suse.de
- update: version 1.2.1.
* Thu Nov 05 1998 - ke@suse.de
- use the TDS and provide links from /usr/share/lilypond.
- install examples via %doc.
* Wed Oct 28 1998 - ke@suse.de
- update: version 1.0.17.
* Fri Aug 07 1998 - ke@suse.de
- initial package: version 1.0.0
