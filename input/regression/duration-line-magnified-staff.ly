\version "2.23.8"

\header {
  texidoc = "Duration lines are placed vertically correct for non-default staff sizes and all styles."
}

\layout {
  \context {
    \Voice
    \consists "Duration_line_engraver"
  }
}


staffSize =
#(define-music-function (new-size) (number?)
   #{
     \set fontSize = #new-size
     \override StaffSymbol.staff-space = #(magstep new-size)
     \override StaffSymbol.thickness = #(magstep new-size)
   #})


testNotes = \relative c'' { <a a, a''>2\- <a a, a''>2 }


testContent = \relative c'' {
  \testNotes
  \override DurationLine.style = #'zigzag
  \testNotes
  \override DurationLine.style = #'beam
  \testNotes
  \break

  \override DurationLine.style = #'trill
  \testNotes
  \override DurationLine.style = #'dashed-line
  \testNotes
  \override DurationLine.style = #'dotted-line
  \testNotes
}

\score {
  <<

    \new Staff \with { instrumentName = \markup{\rotate #90 \tiny "Standard"}  }  % This one works as the size is standard
    \testContent

    \new StaffGroup \with { instrumentName = \markup{\rotate #90 \tiny "\\magnifyStaff"} }
    <<
      \new Staff \with { \magnifyStaff #2 }
      \testContent
      \new Staff \with { \magnifyStaff #0.6 }
      \testContent
    >>

    \new StaffGroup \with { instrumentName = \markup{\rotate #90 \tiny "\\staffSize"} }
    <<
      \new Staff \with { \staffSize #3 }
      \testContent
      \new Staff \with { \staffSize #-3 }
      \testContent
    >>
  >>
}
