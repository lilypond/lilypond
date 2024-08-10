\version "2.25.16"
\header  {
  texidoc = "Feathered beams should space noteheads closer when they have
  more beams."
  
}

\version "2.23.10"

\paper {
  ragged-right = ##t
  indent = #0.0
}

<<
  \new Staff \featherDurations 3/4 \relative {
    \override Beam.grow-direction = #LEFT
    c''16[ c c c c c c c ]
  }
  \new Staff \relative { c''16[ c c c c c c c ] }
>>
