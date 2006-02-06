
\version "2.7.32"

\header{

  texidoc="Automatic cross-staff knees work also (here they were
produced with explicit staff switches)."

}

\layout{
  ragged-right = ##t
}

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
