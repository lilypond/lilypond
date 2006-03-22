\version "2.8.0"
\header {

  texidoc = "Unterminated piano pedal brackets run to the end of the piece. "

}

\layout { ragged-right  = ##t }


{
  \set Staff.pedalSustainStyle = #'bracket
  c4 \sustainDown
  \bar "|."
}


