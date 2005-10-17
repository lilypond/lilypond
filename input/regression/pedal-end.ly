\version "2.7.13"
\header {

  texidoc = "Unterminated piano pedal brackets run to the end of the piece. "

}

\layout { raggedright  = ##t }


{
  \set Staff.pedalSustainStyle = #'bracket
  c4 \sustainDown
  \bar "|."
}


