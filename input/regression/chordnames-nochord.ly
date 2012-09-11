\version "2.16.0"

\header {
  texidoc = "Rests in music passed to ChordNames context display noChordSymbol.
noChordSymbol is treated like a ChordName with respect to chordChanges.
"
}

myChords = \chordmode {
  c1 r1 r1 \break
  r1 g1 c1 \break
}

\score {
  <<
    \new ChordNames { 
      \myChords 
      \set chordChanges = ##t
      \myChords
    }
    \new FretBoards { 
      \myChords 
      \myChords
    }
    \new Staff { 
      \myChords 
      \myChords
    }
  >>
}
