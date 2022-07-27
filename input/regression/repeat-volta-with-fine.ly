\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\fine} ends the piece when it is found outside
folded repeats."
}

piece = \new Voice \fixed c' {
  \repeat volta 2 {
    f1
    \volta 2 \fine
    \volta 1 b1
  }
}

\score { \piece }
\score { \unfoldRepeats \piece }
