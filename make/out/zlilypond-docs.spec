#
# The weird name is to persuade RPM to use the other spec for building the 
# lilypond rpm from the tarball.
#

Name: lilypond-docs
Version: 1.0.1
Release: 1
Copyright: GPL
Group: Applications/Publishing
Source0: pcnov095.win.tue.nl:/pub/lilypond/development/lilypond-1.0.1.tar.gz
Summary: A program for typesetting music (documentation)
URL: http://www.cs.uu.nl/~hanwen/lilypond
Packager: Han-Wen Nienhuys <hanwen@cs.uu.nl>
Icon: lelie_icon.gif
BuildRoot: /tmp/lilypond-install
BuildArchitectures: noarch


%description 

LilyPond is the GNU Project music typesetter.  This program can print
beautiful sheet music from a music definition file.  It can also play
mechanical performances to a MIDI file.  Features include multiple
staffs, meters, clefs, keys, lyrics, versatile input language, cadenzas,
beams, slurs, triplets, formatting scores, part extraction.  It includes
a nice font of musical symbols.

This package contains the documentation in HTML and PS format.

%prep
%setup  -T -n lilypond-1.0.1 -b0
%build

#
# This is actually quite gross.  You have to have a working (and up to date)
# LilyPond and Yodl  to  build the documentation.
#

make htmldocs

%install

# ln /home/hanwen/usr/src/lilypond/out/htmldoc.tar.gz out/
mkdir -p $RPM_BUILD_ROOT/usr/doc/lilypond-1.0.1 
tar -C $RPM_BUILD_ROOT/usr/doc/lilypond-1.0.1 -xzf out/htmldoc.tar.gz 

%files

%doc /usr/doc/lilypond-1.0.1

%post
