\header
{

  texidoc = "With grid lines, vertical lines can be drawn between
staves synchronized with the notes."

}
\version "2.5.21"

skips =
{
\time 12/8
\once  \override Score.GridLine #'thickness = #4.0
  s8 
  s8 
  s8 
\once  \override Score.GridLine #'thickness = #2.0
  s8 
  s8 
  s8 
\once  \override Score.GridLine #'thickness = #4.0
  s8 
  s8 
  s8 
\once  \override Score.GridLine #'thickness = #2.0
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
  gridInterval = #(ly:make-moment 1 8)
  \override BarLine #'bar-size = #0.05
}
  \context {
    \StaffGroup
    \remove "System_start_delimiter_engraver" 
  }
}

\new Score 
\with {
  \consists "Grid_line_span_engraver"
  \override SystemStartBrace #'transparent = ##t
  \override TimeSignature #'transparent = ##t

  \override NoteColumn #'X-offset-callbacks = #(list (lambda (x a) -0.5))
  \override NoteColumn #'Y-offset-callbacks = #(list (lambda (x a) 0.25))

}
\new StaffGroup <<
  \new Staff {
    \repeat unfold 12 { c''8 }
    }
  \new RhythmicStaff \skips
  \new RhythmicStaff \skips
>>
  
