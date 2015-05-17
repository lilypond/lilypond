\version "2.19.21"

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
      \relative {
        a'4 a a a
      }
    }
    \new Staff {
      \clef "bass"
      \relative {
        a,4 a a a
      }
    }
   >>
}
