\version "2.23.4"

\header {
  texidoc = "Music functions that scale durations also scale
@code{\\partCombine} decisions."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
  \context {
    \Voice
    %% This test focuses narrowly on scaling the part combiner's
    %% timing data.  Avoid warnings due to the Tuplet_engraver's
    %% inability to follow the music as the part combiner shifts it
    %% between contexts.
    \remove "Tuplet_engraver"
  }
}

pcmus = \fixed c' {
  \partCombine
  { e2 \grace c8 e2 e2 }
  { c2 \grace f'8 c2 f2 }
}

expected = \fixed c' {
  \partCombine
  { e2*2/3 \grace c8*2/3 e2*2/3 e2*2/3 }
  { c2*2/3 \grace f'8*2/3 c2*2/3 f2*2/3 }
}

\new Score <<
  \new Staff \with { instrumentName = "expected" } {
    \expected
  }

  \new Staff \with { instrumentName = "scale" } {
    \scaleDurations 2/3 \pcmus
  }

  \new Staff \with { instrumentName = "times" } {
    \times 2/3 \pcmus
  }

  \new Staff \with { instrumentName = "tuplet" } {
    \tuplet 3/2 \pcmus
  }
>>
