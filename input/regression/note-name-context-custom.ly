\version "2.21.0"

\header {
  texidoc = "Note names may be printed in various languages,
with or without accidentals and octave marks."
}

\layout {
  \context {
    \Lyrics
    fontSize = #-1
  }
}

\score {
  \language "english"

  <<
    \new Staff {
      af'1
      bff' cs'' <df' f' af'>
      es'
    }

    \new NoteNames \with {
      \override NoteName.self-alignment-X = #LEFT
    } {
      af
      \set printAccidentalNames = ##t
      bff cs <df f af>
      \set printAccidentalNames = #'lily
      es
    }

    \new Lyrics \with {
      instrumentName = "(ref.)"
    } \lyricmode {
      la si♭♭ do♯ re♭+fa+la♭ mid
    }
  >>

  \layout {
    \context {
      \NoteNames
      printNotesLanguage = "italiano"
      printAccidentalNames = ##f
      noteNameSeparator = "+"
    }
  }
}


\score {
  \language "italiano"

  <<
    \new Staff {
      do'1 red' mi'
      la''
    }

    \new NoteNames \with {
      printOctaveNames = ##t
      printNotesLanguage = "français"
    } {
      do' red' mi'
      \set printOctaveNames = #'scientific
      la''
    }

    \new Lyrics \with {
      instrumentName = "(ref.)"
    } \lyricmode {
      do' ré♯' mi' "la5"
    }
  >>
}
