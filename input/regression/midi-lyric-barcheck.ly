\version "2.19.2"
\header {

    texidoc = "Lyrics in MIDI are aligned to ties and beams:
this examples causes no bar checks in MIDI.
"


    }
\score {

    <<\relative c'' \new Voice =  A {
	\autoBeamOff
	c8[ c] c2.
	c1~4 c2.
	c4 ( d e) d
	c1

    }
    \new Lyrics \lyricsto "A" { bla bla | bla bla | bla bla | bla }
    >>
    \layout {}
      \midi {}
    }
