\version "2.3.4"
\header {

    texidoc =
"

In improvisation, noteheads do not have a pitch, and have different
shapes. In this example, this is achieved by adding
@code{Pitch_squash_engraver} and setting @code{squashedPosition} when the
improvisation is active.

"

}


improOn =  {
    \set squashedPosition = #0
    \override NoteHead  #'style = #'slash
}

improOff =  {
    \unset squashedPosition 
    \revert NoteHead #'style
}

global =  { s1*3 \bar "|." }

\score {
    <<
	\context ChordNames \chords {
	    e8*7:m7 a2.:m7 bes4:m7 b1:m7 e8:m
	}
	 <<
	    \context Voice = melo \transpose c c' {
		e8 e g a a16(bes)(a8) g \improOn
		e8
		~e2~e8 f4 fis8
		~fis2 \improOff a16(bes) a8 g e
	    }
	>>
    >>
    \paper { 
	\context {
	    \Voice
	    \consists Pitch_squash_engraver
	}
	raggedright = ##t
    }
}

