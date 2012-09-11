
\version "2.16.0"
\header { texidoc = "@cindex Beam Damp
Beams are less steep than the notes they encompass. " }
\layout{
  ragged-right = ##t
}

\relative c''{
  %%		\stemUp
  %%		 a16[ b b c]
  %%		 c[ b b a]
  %%		\stemDown
  %%		 c[ b b a]
  %%		 a[ b b c]
  \stemUp
  g16[ a b c]
  c[ b a g]
  \stemDown
  d'[ c b a]
  a[ b c d]
}


