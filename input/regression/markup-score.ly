\header {
    texidoc = "Use \\score block as markup command."
}

\version "2.19.21"

tuning = \markup {
    \score {
	\new Staff \with {
	    \remove "Time_signature_engraver"
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
    \tuplet 3/2 { c''8 d e } \tuplet 3/2 {c d e}
    \tuplet 3/2 { c8 d e } \tuplet 3/2 {c d e}
    g8 a8 g8 a
    g8 a8 g8 a
}

