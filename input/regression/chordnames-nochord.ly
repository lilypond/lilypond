\version "2.27.0"

\header {
  texidoc = "Rests in music passed to a @code{ChordNames} context display
the markup stored in the @code{noChordSymbol} property.
@code{noChordSymbol} is treated like a chord name with respect to chord
changes.

A single-note chord over a bass with the same pitch also uses
@code{noChordSymbol}, replacing the root name."
}

myChords = \chordmode {
  c1 r1 R1 g1:1/g g1:1/g \break
  r1 g1 g1 c1 c1 \break
}

myScore = <<
  \new FretBoards \myChords
  \new ChordNames \myChords
  \new Staff \myChords
>>

\score {
  \myScore
}
\markup \typewriter "\\set chordChanges = ##t"
\score {
  \myScore
  \layout {
     \context {
       \ChordNames
       chordChanges = ##t
     }
  }
}
