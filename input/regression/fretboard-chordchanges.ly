\header{
  
  texidoc = "FretBoards can be set to display only when the chord changes
or at the beginning of a new line."

}
  
  \version "2.13.0"

myChords = \chordmode {
  c1 c1 \break
  c1 c1 \break
  \set chordChanges = ##t
  c1 c1 \break
}

<<
  \new ChordNames { \myChords }
  \new FretBoards { \myChords }
  \new Staff { \myChords }
>>
