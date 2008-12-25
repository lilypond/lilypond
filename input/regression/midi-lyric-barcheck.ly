\version "2.12.0"
\header {

    texidoc = "Lyrics in MIDI are aligned to ties and beams:
this examples causes no bar checks in MIDI.
"


    }
\score {

    <<\relative c'' \new Voice =  A {
	\autoBeamOff
	c8[ c] c2.
	c1~c4 c2.
	c4 ( d e) d
	c1

    }
    \lyricsto "A" \lyrics { bla bla | bla bla | bla bla | bla }
    >>
    \layout {}
      \midi {}
    }
