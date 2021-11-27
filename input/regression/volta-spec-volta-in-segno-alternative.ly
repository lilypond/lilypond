\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="In a @code{\\repeat segno}, the @code{\\volta}
specifications of the elements of a tail @code{\\alternative} do not
create volta brackets; however, deeper @code{\\volta} specifications
still create brackets.  A bracket ending at a @emph{D.C.} hooks down."
}

piece = \new Voice \fixed c' {
  \repeat segno 3 {
    f1
    { % This extra {} shows that it isn't necessary for the
      % \alternative music to be the child of the \repeat music.
      \alternative {
        \volta 1,2 {
          g1
          <<
            \once \override Score.VoltaBracket.text = "2nd time tacet"
            \once \override Score.VoltaBracket.font-name = "LilyPond Serif"
            \volta 1 { f4 4 4 4 }
            \volta 2 \unfolded R1
          >>
        }
        \volta 3 <>
      }
    }
  }
  \section \sectionLabel "Coda"
  b1
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
