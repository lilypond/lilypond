\version "1.5.68"
\header {
  filename =  "title.ly"
  dedication = "dedication"
  title = "Title"
  subtitle = "Subtitle"
  subsubtitle = "Subsubtitle"
  composer = "Composer (xxxx-yyyy)"
  instrument = "Instrument"
  arranger = "Arranger"
  poet = "Poet"
  % ugr: warning: Identifier name is a keyword: `translator'
  % translator = "Translator"
  texttranslator = "Translator"
  copyright = "public domain"
  enteredby = "jcn"
  source =  "urtext"
}

\score {
  \context Staff \notes \relative c' {
     \repeat unfold 10 {
       c d e f f e d c \break
       c d e f f e d c
     }
  }
  \header {
    opus = "Opus 0"
    piece = "Piece I"
  }
}

\score {
  \notes \relative c' {
     f e d c c d e f \break
     f e d c c d e f
  }
  \header { 
    opus = "Opus 1"
    piece = "Piece II" 
  }
}

