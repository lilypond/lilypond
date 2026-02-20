\version "2.21.0"

\header {
  texidoc = "
The English naming of chords (default) can be changed to German
(@code{\\germanChords} replaces B and Bes with H and B), semi-German
(@code{\\semiGermanChords} replaces B and Bes with H and Bb), Italian
(@code{\\italianChords} uses Do Re Mi Fa Sol La Si), or French
(@code{\\frenchChords} uses Do Ré Mi Fa Sol La Si).
"
}

scm = \chordmode {
  e1/d c:m b/b bis/bis bes/bes beses/beses
}

\layout {
  indent = 3\cm
  ragged-right = ##t

  \context {
    \ChordNames
    \consists "Instrument_name_engraver"
  }
  \context {
    \Score
    \override InstrumentName.self-alignment-Y = -1.2
    \override InstrumentName.self-alignment-X = #RIGHT
  }
}

<<
  \new ChordNames {
    \set instrumentName = "default"
    \scm
  }
  \new ChordNames {
    \set instrumentName = "German"
    \germanChords \scm }
  \new ChordNames {
    \set instrumentName = "semi-German"
    \semiGermanChords \scm }
  \new ChordNames {
    \set instrumentName = "Italian"
    \italianChords \scm }
  \new ChordNames {
    \set instrumentName = "French"
    \frenchChords \scm }

  \context Voice \scm
>>
