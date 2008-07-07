
\version "2.11.51"

\header {

  texidoc = "In the single tie case, broken ties peek across line
  boundaries to determine which direction to take."

}


\paper {
  ragged-right = ##t
}

\relative c'' {
 bes1~ \break
 bes2.
 \stemUp bes4 ~ \break
 bes1 
}
