\version "2.19.9"
#(set-global-staff-size 30)

\header {
  texidoc = "Lyrics without an @code{associatedVoice} should align properly.
If there are notes in the @code{PaperColumn}, they should align to them,
and when there are no notes, they should align relative to the
@code{PaperColumn} itself (represented with blue @code{GridLines} here)"
}

\paper {
  ragged-right = ##f
}

\layout {
  \context {
    \Voice
    \consists "Grid_point_engraver"
    gridInterval = #(ly:make-moment 1/4)
    \override GridPoint.Y-extent = #'(-1 . 3)
  }
  \context {
    \Staff
    \consists "Grid_line_span_engraver"
    \override GridLine.color = #blue
  }
}

music = <<
  \new Staff <<
    \new Voice { s1*3 } % needed for gridLines
    \new Voice { d'2 d' <f' g'>1 s1 }
  >>
  \new Lyrics { \lyricmode { foo2 bar mmmm1 a2 bom } }
>>

\markup "default (centered):"
\music

\markup "right-aligned:"
{
  \override Score.LyricText.self-alignment-X = #RIGHT
  \music
}
