\version "2.6.0"

\header  {
texidoc = "a staff should really die, if no one's referencing it."
}
\score {
 {
    \new Staff =  "q" {
       { a' b' c' d' }
    }

    \break

    \context PianoStaff <<
      \new Staff =  "i" {
         { a' b' c' d' }
      }
      \new Staff =  "ii" {
         { \clef "bass" a b c d }
      }
    >>
  }
}
