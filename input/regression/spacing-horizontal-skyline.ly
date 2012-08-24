
\header {

  texidoc = "accidentals may be folded under preceding notes."
  }
\version "2.16.0"

\paper {
  ragged-right = ##t
  }

\relative c''' {
  \stemUp c4...*1/2

  ceses,,!
  eses!
  ceses!
  feses!
  ceses!
  geses'!
  ceses,!

}

