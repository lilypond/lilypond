\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="A @code{\\repeat segno} with a single alternative ending
that is used for all volte receives a volta bracket rather than a coda
sign because there is no material to skip.  The bracket hooks down at
the @emph{D.C.}.

The bracket communicates the return count, so the return count is
omitted from the @emph{D.C.} instruction to avoid redundancy."
}

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    f1
    \alternative {
      \volta 1,2 {
        g1
      }
    }
  }
  \sectionLabel "Coda"
  a1
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
