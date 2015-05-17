
\version "2.19.21"

\header {

  texidoc = "In the single tie case, broken ties peek across line
  boundaries to determine which direction to take."

}


\paper {
  ragged-right = ##t
}

\relative {
 bes'1~ \break
 bes2.
 \stemUp bes4 ~ \break
 bes1 
}
