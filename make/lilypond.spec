Name: lilypond
Version: 0.0.52
Release: 1
Copyright: GPL
Group: Applications/Publishing
Source0: pcnov095.win.tue.nl:/pub/lilypond/lilypond-0.0.52.tar.gz
Summary: A preprocessor to make TeX typeset music.
URL: http://www.stack.nl/~hanwen/lilypond
Packager: Han-Wen Nienhuys <hanwen@stack.nl>
Icon: lelie_icon.gif
Buildroot: /tmp/lilypond_build

%description
LilyPond is a program which converts a music-script (mudela) into
TeX output, or MIDI to produce multi-staff scores. Features include multiple
meters, clefs, keys, lyrics, versatile input-language, cadenzas
beams, slurs, triplets, multi voices.

%prep
%setup
%build
configure --enable-checking --enable-printing --prefix=/usr --enable-optimise
make all
%install
strip bin/lilypond bin/mi2mu
make prefix="$RPM_BUILD_ROOT/usr" install
%files
%doc Documentation/out/CodingStyle.text Documentation/out/INSTALL.text Documentation/out/MANIFESTO.text Documentation/out/error.text Documentation/out/examples.text Documentation/out/faq.text Documentation/out/index.text Documentation/out/language.text Documentation/out/lilygut.text Documentation/out/lilypond.text Documentation/out/mudela.text Documentation/lelie_logo.gif
/usr/bin/lilypond
/usr/bin/mi2mu
/usr/man/man1/lilypond.1
/usr/man/man5/mudela.5
/usr/lib/texmf/texmf/tex/lilypond/
/usr/share/lilypond/

%post
texhash
%post
texhash

