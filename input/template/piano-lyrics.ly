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

upper = \notes\relative c'' {
  a b c d
}

lower = \notes\relative c {
  a2 c
}

text = \lyrics {
  Aaa Bee Cee Dee
}

\score {
  \context GrandStaff <
    \addlyrics
      \context Staff = upper \upper
      \context Lyrics \text
    \context Staff = lower <
      \clef bass
      \lower
    >  
  >
  \paper {
    \translator {
      \GrandStaffContext
      \accepts "Lyrics"
    }
    \translator {
      %\LyricsVoiceContext
      \LyricsContext
      \consists "Bar_engraver"
    }
  }  
  \midi { }  
}