\version "2.3.16"
\header {

    texidoc = "Unterminated piano pedal brackets run to the end of the piece. "

}

\score {
     {
	\set Staff.pedalSustainStyle = #'bracket
	c4 \sustainDown
	\bar "|."
    }
    \paper { raggedright  = ##t }
}
