\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This case nests one @code{\\alternative} within another at
the tail end of a @code{\\repeat segno}.  Alternative bar numbering is
enabled.

The outer alternative receives a coda mark, no volta bracket, and
normal bar numbering.

The inner alternative receives volta brackets.  Alternative bar
numbering is used because they are the outermost volta brackets.

The music unfolds to ABC ABD AE."
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
            \volta 1 s1_"C"
            \volta 2 s1_"D"
          }
        }
        \volta 3 <>
      }
    }
  }
  \sectionLabel "Coda"
  s1_"E"
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
