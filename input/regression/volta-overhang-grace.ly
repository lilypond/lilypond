\version "2.25.23"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="A final volta bracket overhanging the next section can be achieved
by overriding @code{Score@/.VoltaBracket@/.musical-length} in a zero-duration
alternative.  The bracket for volta 2 should end halfway through the final
measure, before the grace notes."
}

music = \context Voice \fixed c' {
  \repeat volta 2 {
    s1_"A"
    \alternative {
      s1_"B"
      \once \override Score.VoltaBracket.musical-length = \musicLength 2*3
    }
  }
  s1_"C" |
  d2
  \grace { e32 f g }
  a2
}

\score { \music }
\score { \unfoldRepeats \music }
