Name: lilypond
Version: 0.1.59
Release: 1
Copyright: GPL
Group: Applications/Publishing
Source0: pcnov095.win.tue.nl:/pub/lilypond/development/lilypond-0.1.59.tar.gz
Summary: A program for typesetting music.
URL: http://www.cs.ruu.nl/~hanwen/lilypond
Packager: Han-Wen Nienhuys <hanwen@stack.nl>
Icon: lelie_icon.gif
Buildroot: /tmp/lilypond-install

%description 
LilyPond is the GNU Project music typesetter.  The program
generates visual or auditive output from a music 
definition file: it can typeset formatted sheet music 
to a TeX file and play (mechanical) performances to a 
MIDI file.  Features include multiple staffs, meters, 
clefs, keys, lyrics, versatile input-language, 
cadenzas, beams, slurs, triplets.
It includes a nice font of musical symbols.

%prep
%setup
%build
./configure --disable-checking --disable-debugging --enable-printing --prefix=/usr --enable-optimise --enable-shared
make all
%install
rm -rf $RPM_BUILD_ROOT
strip lily/out/lilypond mi2mu/out/mi2mu
make -C Documentation gifs
make prefix="$RPM_BUILD_ROOT/usr" install
%files
%doc Documentation/out/AUTHORS.txt Documentation/out/CodingStyle.txt Documentation/out/INSTALL.txt Documentation/out/MANIFESTO.txt Documentation/out/PATCHES.txt Documentation/out/faq.txt Documentation/out/gnu-music.txt Documentation/out/index.txt Documentation/out/internals.txt Documentation/out/language.txt Documentation/out/links.txt Documentation/out/literature.txt Documentation/out/mutopia.txt Documentation/out/other-packages.txt BUGS TODO NEWS DEDICATION ANNOUNCE README
%doc input/cadenza.ly input/coriolan-alto.ly input/dummy.tex input/keys.ly input/kortjakje.ly input/multi.ly input/part.ly input/pedal.ly input/praeludium-fuga-E.ly input/rhythm.ly input/scales.ly input/scripts.ly input/tril.ly input/twinkle-pop.ly input/twinkle.ly Documentation/mudela.doc 
%doc Documentation/out/lelie_logo.gif
/usr/bin/convert-mudela
/usr/bin/mudela-book
/usr/bin/ly2dvi
/usr/bin/lilypond
/usr/bin/mi2mu
# /usr/lib/libflower.so
/usr/man/man1/mi2mu.1
/usr/man/man1/lilypond.1
/usr/man/man1/mudela-book.1
/usr/man/man1/ly2dvi.1
/usr/man/man1/convert-mudela.1
/usr/lib/texmf/texmf/tex/lilypond/
/usr/lib/texmf/texmf/fonts/source/public/lilypond
/usr/share/lilypond/
%post

texhash		# takes some time...
rm `find /var/lib/texmf -name 'feta*pk' -or -name 'feta*tfm'`

