
\version "2.3.17"

\header{

    texidoc="Automatic cross-staff knees work also (here they were
produced with explicit staff switches)."

     }

\score {
   \context PianoStaff <<
    \context Staff = "up" \relative c''{
      b8[ \change Staff="down" d,, ]
      c[ \change Staff="up" c'' ]
      b,[ \change Staff="down" d^"no knee" ]
    }
    \context Staff = "down" {
      \clef bass 
      s2.
    }
  >>
  \paper{
    raggedright = ##t
  }
}
