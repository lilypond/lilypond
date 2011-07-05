\version "2.14.0"

\header {
  lsrtags = "rhythms"

  texidoc = "
Beams can be subdivided automatically.  By setting the property
@code{subdivideBeams}, beams are subdivided at beat positions (as
specified in @code{baseMoment}).

"
  doctitle = "Automatic beam subdivisions"
}

\new Staff {
  \relative c'' {
    <<
      {
        \voiceOne
        \set subdivideBeams = ##t
        b32[ a g f c' b a g
        b32^"subdivide beams" a g f c' b a g]
      }
      \new Voice {
        \voiceTwo
        b32_"default"[ a g f c' b a g
        b32 a g f c' b a g]
      }
    >>
    \oneVoice
    \set baseMoment = #(ly:make-moment 1 8)
    \set beatStructure = #'(2 2 2 2)
    b32^"baseMoment 1 8"[ a g f c' b a g]
    \set baseMoment = #(ly:make-moment 1 16)
    \set beatStructure = #'(4 4 4 4)
    b32^"baseMoment 1 16"[ a g f c' b a g]
  }
}

