\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests simple @emph{D.C.} form with a segno following,
and how it unfolds."
}

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    a1
  }
  \repeat segno 2 {
    b1
  }
}

\score { \piece }
\score { \unfoldRepeats \piece }
