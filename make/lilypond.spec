Description: LilyPond is a program which converts a music-script (mudela) into\
TeX output, or MIDI to produce multi-staff scores. Feature include multiple\
meters, clefs, keys, lyrics, versatile input-language, cadenzas\
beams, slurs, triplets\
multiple voices within one staff.
Name: lilypond
Version: 0.0.39.hwn2
Release: 1
Copyright: GPL
Group: Applications/Publishing
Source0: pcnov095.win.tue.nl:/pub/lilypond/lilypond-0.0.39.hwn2.tar.gz
Summary: A preprocessor to make TeX typeset music.

%prep
%setup
%build
PREFIX=/usr/lib ./configure
make all OPTIFLAG="-O2" 
%install
make install
%files
%doc Documentation/README.txt Documentation/CodingStyle.txt
%doc Documentation/lilygut.txt Documentation/lilyinput.txt
%doc Documentation/error.txt Documentation/faq.txt Documentation/index.txt
%doc Documentation/language.txt Documentation/lelie_logo.png
/usr/bin/lilypond
/usr/bin/mi2mu
/usr/lib/lilypond/init/bare.ini
/usr/lib/lilypond/init/dutch.ini
/usr/lib/lilypond/init/english.ini
/usr/lib/lilypond/init/script.ini
/usr/lib/lilypond/init/swedish.ini
/usr/lib/lilypond/init/symbol.ini
/usr/lib/lilypond/init/table_sixteen.ini
/usr/lib/lilypond/init/table_twenty.ini
/usr/lib/lilypond/titledefs.tex
/usr/lib/lilypond/lilyponddefs.tex



