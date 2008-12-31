\header
{

  texidoc = "With grid lines, vertical lines can be drawn between
staves synchronized with the notes."

}
\version "2.12.0"

skips =
{
  \time 12/8
  \once  \override Score.GridLine #'thickness = #4.0
  s8 
  s8 
  s8 
  \once  \override Score.GridLine #'thickness = #3.0
  s8 
  s8 
  s8 
  \once  \override Score.GridLine #'thickness = #4.0
  s8 
  s8 
  s8 
  \once  \override Score.GridLine #'thickness = #3.0
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
    \override VerticalAxisGroup #'minimum-Y-extent = #'(-4 . 4) 
    gridInterval = #(ly:make-moment 1 8)
    \override BarLine #'bar-size = #0.05
  }
  \context {
    \StaffGroup
    \remove "System_start_delimiter_engraver" 
  }
}

\layout {
  ragged-right = ##t
}
\new Score 
\with {
  \consists "Grid_line_span_engraver"
  \override SystemStartBrace #'transparent = ##t

  \override NoteColumn #'X-offset = #-0.5
  \override NoteHead #'Y-offset = #0.75

}
\new StaffGroup <<
  \new RhythmicStaff \with
  {
    \override NoteHead  #'no-ledgers = ##t
  }
  {
    \stemUp
    c4. c8 c8 c c4 c8 c8.[ c16 c8]
  }
  \new RhythmicStaff
  {
    \override NoteHead #'transparent = ##t
    \override NoteHead #'no-ledgers = ##t
    \override Stem #'transparent = ##t
    \override Beam #'transparent = ##t
    << \skips

				% force regular spacing by introducing notes.
       \repeat unfold 12 c8
     >>
  }
  
>>

