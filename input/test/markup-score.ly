
\header {
    texidoc = "Use \\score block as markup command."
}

\version "2.3.2"

tuning = \markup {
    \score {
	\new Staff \with {
	    \remove Time_signature_engraver
	} {
	    \clef bass  <c, g, d g>1
	}
	\paper { raggedright = ##t }
    }
}


\header {
    title = "Solo Cello Suites"
    subtitle = "Suite IV"
    subsubtitle = \markup { \fill-line < { "Originalstimmung: " \tuning } > }
}

\relative {
    \time 4/8
    \times 2/3 { c'8 d e } \times 2/3 {c d e}
    \time 4/8

				% todo: tempo change example.
    
    g8 a8 g8 a \break
}

