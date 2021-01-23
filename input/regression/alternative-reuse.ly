\version "2.23.1"

\header {
  texidoc="@code{\\alternative} music can be assigned to a variable
and used in multiple places, even with different repeat counts."
}

#(ly:set-option 'warning-as-error #t)

alts = \alternative { s4_"V" s4_"I" }

piece = \fixed c' {
  \time 1/4
  \repeat volta 3 { \alts s4 }
  \repeat volta 2 { \alts s4 }
  \repeat volta 3 { \alts s4 }
}

\new Score \piece
\new Score \unfoldRepeats \piece
