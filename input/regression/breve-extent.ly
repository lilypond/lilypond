\version "2.25.35"

\header  {
  texidoc = "LilyPond knows that breves and longas are wider than whole notes
  (because of vertical lines on their sides).  Breves and longas don't collide
  with accidentals, bar lines, neighbor notes, etc.  The distance between
  accidental and note is the same for whole notes, breves and longas."
}

{
  gis'1
  gis'\breve*1/2
  gis'\longa*1/4
  \override NoteHead.style = #'altdefault
  gis'\breve*1/2
}

\score {
    \new Staff {
      \*8 { a'\breve*1/16 }
      \override NoteHead.style = #'altdefault
      \*8 { a'\breve*1/16 }
    }
    \layout {
        \context {
            \Score
            \override SpacingSpanner.common-shortest-duration = \musicLength 1
        }
    }
}
