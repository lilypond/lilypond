\version "2.27.5"

\header {
  texidoc = "When @code{chordChanges} is active, chord name repetitions are
suppressed except at the beginning of each line.  In the presence of
grace notes, this line beginning is delayed to the first non-grace
moment." }

\layout { ragged-right = ##t }

<<
  \new ChordNames {
    \chordmode { \set chordChanges = ##t
      e1 e e e
    }
  }
  \new Staff {
    e1 \acciaccatura fis'8 e1
    \bar "||"
    e1\break
    \acciaccatura fis'8
    e1
  }
>>

<<
  \new ChordNames {
    \chordmode { \set chordChanges = ##t
      e1 d e d
    }
  }
  \new Staff {
    e1 \acciaccatura fis'8 e1
    \bar "||"
    e1\break
    \acciaccatura fis'8
    e1
  }
>>
