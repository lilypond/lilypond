
\header  {
texidoc = "a staff should really die, if no one's referencing it."
}
\score {
 {
    \context Staff = q {
      \notes { a' b' c' d' }
    }

    \break

    \context PianoStaff <
      \context Staff = i {
        \notes { a' b' c' d' }
      }
      \context Staff = ii {
        \notes { \clef "bass"; a b c d }
      }
    >
  }
}
