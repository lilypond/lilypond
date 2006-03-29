\version "2.7.39"

\header {
    texidoc = " Easy-notation (or Ez-notation) prints names in note heads.
You also get ledger lines, of course."
}


\layout {
  ragged-right = ##t
} 

\relative {
  \setEasyHeads
  f1 e
  f2 e
  f4 e
  
  \override NoteHead #'note-names = ##("U" "V" "W" "X" "Y" "Z" "z")
  c2 d4 e 
}

