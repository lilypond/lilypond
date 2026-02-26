\version "2.25.35"

\layout {
  indent = 3\cm
  ragged-right = ##t

  \context {
    \ChordNames
    \consists "Instrument_name_engraver"
    \override VerticalAxisGroup
              .nonstaff-relatedstaff-spacing.padding = 2
    \override VerticalAxisGroup
              .nonstaff-nonstaff-spacing.minimum-distance = 5.5
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
    \set instrumentName = "Norwegian"
    \norwegianChords \scm }
  \new ChordNames {
    \set instrumentName = "Italian"
    \italianChords \scm }
  \new ChordNames {
    \set instrumentName = "French"
    \frenchChords \scm }
  \new ChordNames {
    \set instrumentName = "English"
    \englishChords \scm }

  \context Voice \scm
>>
