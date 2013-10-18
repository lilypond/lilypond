\header
{

  texidoc = "With grid lines, vertical lines can be drawn between
staves synchronized with the notes."

}
\version "2.17.30"

skips =
{
  \time 12/8
  \once  \override Score.GridLine.thickness = #4.0
  s8 
  s8 
  s8 
  \once  \override Score.GridLine.thickness = #3.0
  s8 
  s8 
  s8 
  \once  \override Score.GridLine.thickness = #4.0
  s8 
  s8 
  s8 
  \once  \override Score.GridLine.thickness = #3.0
  s8 
  s8 
  s8 
}


\layout {
  \context {
    \Staff
    \consists "Grid_point_engraver"
    
  }

  \context {
    \RhythmicStaff
    \consists "Grid_point_engraver"
    gridInterval = #(ly:make-moment 1/8)
    \override BarLine.bar-extent = #'(-0.025 . 0.025)
  }
  \context {
    \StaffGroup
    \remove "System_start_delimiter_engraver" 
  }
  \context {
    \Score
      \consists "Grid_line_span_engraver"
      \hide SystemStartBrace

      \override NoteColumn.X-offset = #-0.5
      \override NoteHead.Y-offset = #0.75
  }
}

\layout {
  ragged-right = ##t
}
\score {
\new StaffGroup <<
  \new RhythmicStaff \with
  {
    \override NoteHead.no-ledgers = ##t
  }
  {
    \stemUp
    c4. c8 c8 c c4 c8 c8.[ c16 c8]
  }
  \new RhythmicStaff
  {
    \hide NoteHead
    \override NoteHead.no-ledgers = ##t
    \hide Stem
    \hide Beam
    << \skips

				% force regular spacing by introducing notes.
       \repeat unfold 12 c8
     >>
  }
  
>>
}

