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
  source =  "urtext "
  enteredby = "your name here"
  maintainerEmail = "your email here"
  texidoc = "The standard header that ought to be above a file."

}

% insert a score otherwise lilypond-book gets confused. 
\score {
  \notes {
  c4
  }
} 
