\version "2.7.39"
\header {

  texidoc = "Unterminated piano pedal brackets run to the end of the piece. "

}

\layout { ragged-right  = ##t }


{
  \set Staff.pedalSustainStyle = #'bracket
  c4 \sustainDown
  \bar "|."
}


