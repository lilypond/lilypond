\version "2.1.30"
\header {

    texidoc = "Unterminated piano pedal brackets run to the end of the piece. "

}

\score {
    \notes {
	\set Staff.pedalSustainStyle = #'bracket
	c4 \sustainDown
	\bar "|."
    }
    \paper { raggedright  = ##t }
}
