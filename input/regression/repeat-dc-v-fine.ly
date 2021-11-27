\version "2.23.6"

\header {
  texidoc="When a @emph{Fine} and a @emph{D.C.} occur simultaneously,
LilyPond ignores the @emph{Fine} and issues a warning.  Unfolding is
not affected: this case unfolds to only two measures."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (_i "Ignoring Fine simultaneous with D.C. or D.S."))

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    s1
    \volta 2 \fine
  }
  s1
}

\score { \piece }
\score { \unfoldRepeats \piece }
