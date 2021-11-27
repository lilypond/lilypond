\version "2.23.6"

\header {
  texidoc="When jumps to different targets occur simultaneously,
LilyPond ignores one and issues a warning.  Either a @emph{D.C.} or a
@emph{D.S.} instruction, but not both, is expected.  Unfolding is not
affected: this case unfolds to EGGEGG."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme
                     "conflict with event: `%s'") "dal-segno-event")
#(ly:expect-warning (ly:translate-cpp-warning-scheme
                     "discarding event: `%s'") "dal-segno-event")

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    e1
    \repeat segno 2 {
      g1
    }
  }
}

\score { \piece }
\score { \unfoldRepeats \piece }
