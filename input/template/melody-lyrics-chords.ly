\version "1.5.1"

\header {
  dedication = "dedication"
  title = "Title"
  subtitle = "Subtitle"
  subsubtitle = "Subsubtitle"
  composer = "Composer (xxxx-yyyy)"
  opus = "Opus 0"
  piece = "Piece I"
  instrument = "Instrument"
  arranger = "Arranger"
  poet = "Poet"
  texttranslator = "Translator"
  copyright = "public domain"
  enteredby = "jcn"
  source =  "urtext"
}

melody = \notes \relative c' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

accompaniment = \chords {
  a2: c2
}

\score {
  <
    \context ChordNames \accompaniment
    \addlyrics
      \context Staff = one {
        \property Staff.noAutoBeaming = ##t
        \property Staff.automaticMelismata = ##t
        \melody
      }
      \context Lyrics \text
  >
  \paper { }
  \midi  { }
}
