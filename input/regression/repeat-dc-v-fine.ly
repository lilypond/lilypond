\version "2.23.6"

\header {
  texidoc="When events creating @emph{Fine} and @emph{D.C.} occur
simultaneously, both indications are printed.  This use case is not
valued, but it is included in the regression test suite for robustness
and difference detection."
}

#(ly:set-option 'warning-as-error #t)

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    s1
    \volta 2 \fine
  }
  s1
}

\score { \piece }
\score { \unfoldRepeats \piece }
