Name: lilypond
Version: 0.1.1
Release: 1
Copyright: GPL
Group: Applications/Publishing
Source0: pcnov095.win.tue.nl:/pub/lilypond/lilypond-0.1.1.tar.gz
Summary: A preprocessor to make TeX typeset music.
URL: http://www.stack.nl/~hanwen/lilypond
Packager: Han-Wen Nienhuys <hanwen@stack.nl>
Icon: lelie_icon.gif
Buildroot: /tmp/lilypond-install

%description
GNU LilyPond which converts music definition files into visual or
audio output: it can typeset formatted sheet music to a TeX file and
and (mechanical) performances to MIDI files. Features include multiple
staffs meters, clefs, keys, lyrics, versatile input-language, cadenzas
beams, slurs, triplets.

%prep
%setup
%build
./configure --enable-checking --disable-debugging --enable-printing --prefix=/usr --enable-optimise --enable-shared
make all
%install
rm -rf $RPM_BUILD_ROOT
strip lily/out/lilypond mi2mu/out/mi2mu
make prefix="$RPM_BUILD_ROOT/usr" install
%files
%doc Documentation/out/AUTHORS.text Documentation/out/CodingStyle.text Documentation/out/INSTALL.text Documentation/out/MANIFESTO.text Documentation/out/convert-mudela.text Documentation/out/faq.text Documentation/out/gnu-music.text Documentation/out/index.text Documentation/out/language.text Documentation/out/lilygut.text Documentation/out/lilyliterature.text Documentation/out/lilypond.text Documentation/out/links.text Documentation/out/mi2mu.text Documentation/out/mudela-book.text Documentation/out/other-packages.text BUGS TODO NEWS DEDICATION ANNOUNCE README 
%doc input/beams.ly input/cadenza.ly input/collisions.ly input/coriolan-alto.ly input/header.ly input/keys.ly input/kortjakje.ly input/multi.ly input/pedal.ly input/rhythm.ly input/rock.ly input/scales.ly input/scripts.ly input/scsii-menuetto.ly input/scsii-menuetto.tex input/slurs.ly input/standchen.ly input/standchen.tex input/toccata-fuga-E.ly input/twinkle.ly input/wohltemperirt.ly Documentation/mudela-course.doc Documentation/mudela-man.doc 
%doc Documentation/lelie_logo.gif
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
/usr/share/lilypond/
%post

texhash		# takes some time...

