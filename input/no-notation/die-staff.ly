\version "2.3.22"

\header  {
texidoc = "a staff should really die, if no one's referencing it."
}
\score {
 {
    \context Staff = q {
       { a' b' c' d' }
    }

    \break

    \context PianoStaff <<
      \context Staff = i {
         { a' b' c' d' }
      }
      \context Staff = ii {
         { \clef "bass" a b c d }
      }
    >>
  }
}
