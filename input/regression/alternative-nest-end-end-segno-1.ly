\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This case nests one @code{\\alternative} within another at
the tail end of a @code{\\repeat segno}.  Alternative bar numbering is
enabled.

The outer alternative receives a coda mark, no volta bracket, and
normal bar numbering.

The inner alternative receives a volta bracket.  Alternative bar
numbering is used because it is the outermost volta bracket.  The
bracket communicates the return count, so the return count is omitted
from the @emph{D.C.}  instruction to avoid redundancy.

The music unfolds to ABC ABC AD."
}

\layout {
  \context {
    \Score
    alternativeNumberingStyle = #'numbers-with-letters
    \override BarNumber.break-visibility = #all-visible
  }
}

piece = \new Voice \fixed c' {
  \repeat segno 3 {
    s1_"A"
    { % This extra {} shows that it isn't necessary for the
      % \alternative music to be the child of the \repeat music.
      \alternative {
        \volta 1,2 {
          s1_"B"
          \alternative {
            \volta 1,2 s1_"C"
          }
        }
        \volta 3 <>
      }
    }
  }
  \sectionLabel "Coda"
  s1_"D"
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
