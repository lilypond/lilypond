\version "2.16.0"

\header {

  texidoc = "
GrandStaff contexts accept chord names.  The chord name in this
example should be printed above the top staff.
"
}

\score {
   \new GrandStaff
   <<
    \chords {
      f1
    }
    \new Staff {
      \relative c'' {
        a4 a a a
      }
    }
    \new Staff {
      \clef "bass"
      \relative c {
        a4 a a a
      }
    }
   >>
}
