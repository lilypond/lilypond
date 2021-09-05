\version "2.23.4"

\header {
  texidoc = "Demonstrate all titling variables used by default."
}

\paper {
  #(set-paper-size "a6")
}

\header {
  dedication = "Dedication"
  title = "Title"
  subtitle = "Subtitle"
  subsubtitle = "Subsubtitle"
  poet = "Poet"
  instrument = "Instrument"
  composer = "Composer"
  meter = "Meter"
  arranger = "Arranger"
  tagline = "Tagline"
  copyright = "Copyright"
}

\book {
  \bookpart {
    \score {
      \header {
        piece = "Piece"
        opus = "Opus"
      }
      \repeat unfold 3 { c'1 \break }
    }
    \pageBreak
    \score {
      \header {
        piece = "Piece 2"
        opus = "Opus 2"
        breakbefore = ##t
      }
      \repeat unfold 3 { c'1 \break }
    }
  }
  \bookpart {
    \paper {
      print-all-headers = ##t
    }
    \score {
      \header {
        title = "Overridden title"
        piece = "Piece again"
        opus = "Opus"
      }
      { c'1 }
    }
    \score {
      \header {
        piece = "Piece 2 again"
        opus = "Opus 2"
      }
      { c'1 }
    }
  }
}
