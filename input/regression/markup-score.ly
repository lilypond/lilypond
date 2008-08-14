\header {
    texidoc = "Use \\score block as markup command."
}

\version "2.11.51"

tuning = \markup {
    \score {
	\new Staff \with {
	    \remove Time_signature_engraver
	} {
	    \clef bass  <c, g, d g>1
	}
	\layout { ragged-right = ##t }
    }
}


\header {
    title = "Solo Cello Suites"
    subtitle = "Suite IV"
    subsubtitle = \markup { "Originalstimmung:" \tuning }
}

\relative {
    \time 4/8
    \times 2/3 { c'8 d e } \times 2/3 {c d e}
    \times 2/3 { c8 d e } \times 2/3 {c d e}
    g8 a8 g8 a 
    g8 a8 g8 a 
}

