\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests simple @emph{D.C.} form with a rehearsal mark
following."
}

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    a1
  }
  \mark \default
  b1
}

\score { \piece }
\score { \unfoldRepeats \piece }
