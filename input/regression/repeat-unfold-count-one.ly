\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "@code{\\repeat unfold 1} unfolds according to the count.
This piece has one measure and @code{\\unfoldRepeats} does not change
that."
}

piece = \fixed c' {
  \repeat unfold 1 b1
}

\score { \piece }
\score { \unfoldRepeats \piece }
