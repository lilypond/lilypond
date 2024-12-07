\version "2.25.23"

\header {
  lsrtags = "automatic-notation, connecting-notes, rhythms"

  texidoc = "
Beams can be subdivided automatically.  By setting the property
@code{subdivideBeams}, beams are subdivided whenever possible. The
intervals and depth of subdivision can be limited with properties
@code{beamMinimumSubdivision} and
@code{beamMaximumSubdivision} respectively.
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
    \once \set beamMinimumSubdivision = #1/8
    b32^"beamMinimumSubdivision 1/8"[ a g f c' b a g]
    \once \set beamMaximumSubdivision = #1/16
    b32^"beamMaximumSubdivision 1/16"[ a g f c' b a g]
  }
}
