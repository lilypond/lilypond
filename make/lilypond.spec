Name: lilypond
Version: 
Release: 1
Copyright: GPL
Group: Applications/Publishing
Source0: alpha.gnu.org:/gnu/lilypond/development/lilypond-.tar.gz
Summary: A program for typesetting music.
URL: http://www.stack.nl/~hanwen/lilypond
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
%doc Documentation/AUTHORS.txt Documentation/CodingStyle.txt Documentation/INSTALL.txt Documentation/MANIFESTO.txt Documentation/PATCHES.txt Documentation/convert-mudela.txt Documentation/faq.txt Documentation/gnu-music.txt Documentation/index.txt Documentation/internals.txt Documentation/language.txt Documentation/lilypond.txt Documentation/links.txt Documentation/literature.txt Documentation/ly2dvi.txt Documentation/mi2mu.txt Documentation/mudela-book.txt Documentation/mutopia.txt Documentation/other-packages.txt BUGS TODO NEWS DEDICATION ANNOUNCE README
%doc input/abbrev.ly input/beam-bug.ly input/beams.ly input/cadenza.ly input/collisions.ly input/coriolan-alto.ly input/denneboom.ly input/dummy.tex input/font-body.ly input/font.ly input/font11.ly input/font13.ly input/font16.ly input/font20.ly input/font26.ly input/gourlay.ly input/keys.ly input/kortjakje.ly input/multi.ly input/pedal.ly input/praeludium-fuga-E.ly input/rhythm.ly input/scales.ly input/scripts.ly input/sleur.ly input/slurs.ly input/spacing.ly input/stem.ly input/test-lyrics.ly input/tril.ly input/twinkle-pop.ly input/twinkle.ly Documentation/introduction.doc Documentation/mudela-man.doc 
%doc Documentation/out/lelie_logo.gif
/usr/bin/convert-mudela
/usr/bin/mudela-book
/usr/bin/lilypond
/usr/lib/libflower.so
/usr/bin/mi2mu
/usr/man/man1/mi2mu.1
/usr/man/man1/lilypond.1
/usr/man/man1/mudela-book.1
/usr/man/man1/convert-mudela.1
/usr/lib/texmf/texmf/tex/lilypond/
/usr/lib/texmf/texmf/fonts/source/public/lilypond
/usr/share/lilypond/
%post

texhash		# takes some time...
rm `find /var/lib/texmf -name 'feta*pk' -or -name 'feta*tfm'`

