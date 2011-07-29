\version "2.14.0"

\header{
  texidoc="Properties capoPitch, capoVertical: display chordnames, suitably
transposed for a guitar capo, either in a line or one above the other.
"
}

<<
  \new ChordNames \chordmode {
    c1
    g1
    c1
    g1
    c1
    g1
  }
  \chordmode {
    c1
    g1
    \set ChordNames.capoPitch = #(ly:make-pitch 0 -2 -1/2)
    c1
    g1
    \set ChordNames.capoVertical = ##t
    c1
    g1
  }
>>
