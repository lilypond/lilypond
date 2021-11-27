\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Setting @code{segnoStyle} to @code{bar-line} suppresses the
first segno mark and causes a @emph{D.S.} instruction to say simply
@emph{D.S.} without the mark.  The second segno mark does appear and
the corresponding @emph{D.S.} instruction includes it."
}

\layout {
  \context {
    \Score
    segnoStyle = #'bar-line
  }
}

piece = \new Voice \fixed c' {
  e1
  \repeat segno 2 {
    g1
  }
  \repeat segno 2 {
    b1
  }
}

\score { \piece }
\score { \unfoldRepeats \piece }
