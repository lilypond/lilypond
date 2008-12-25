
\version "2.12.0" \header {

    texidoc = "Octavation signs may be added to clefs.  These
octavation signs may be placed below or above (meaning an octave
higher or lower), and can take any value, including 15 for two octaves."

}
\layout { ragged-right = ##t  }


\relative c'' {

  \clef "G_8"
  c4
  \clef "G_15"
  c4
  \clef "G_7"
  c4
  \clef "G_6"
  c4 
  \clef "G^8"
  c4 
  \clef "G^15"
  c4 
  \clef "G^9"
  c4 
}

