\version "2.6.0"
\header {

    texidoc = "Unterminated piano pedal brackets run to the end of the piece. "

}

\score {
     {
	\set Staff.pedalSustainStyle = #'bracket
	c4 \sustainDown
	\bar "|."
    }
    \layout { raggedright  = ##t }
}
