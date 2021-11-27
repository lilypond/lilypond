\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When a @emph{D.C.} or @emph{D.S.} instruction is to be
performed more than once, the default @code{dalSegnoTextFormatter}
includes the count in the instruction.  In this case, the @emph{D.C.}
instruction should indicate returning thrice and the @emph{D.S.}
instruction should indicate returning twice."
}

piece = \new Voice \fixed c' {
  \repeat segno 4 {
    s1_"A"
  }
  \repeat segno 3 {
    s1_"B"
    \alternative {
      \volta 1,2 s1_"C"
      \volta 3 <>
    }
  }
  \sectionLabel "Coda"
  s1_"D"
}

\score { \piece }
\score { \unfoldRepeats \piece }
