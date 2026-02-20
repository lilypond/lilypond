\version "2.21.0"

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
