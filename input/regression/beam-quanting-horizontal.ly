
\version "2.19.21"

\header{

    texidoc = "In this test for beam quant positions for horizontal beams,
staff lines should be covered in all cases. For 32nd beams, the free stem
lengths are between 2 and 1.5."

}

\relative { 
  c'8[ c]  a''[ a]
  a,[ a]  c[ c]
  d,8[ d]  g'[ g]
  g,[ g]  d'[ d]
  \break
  c,16[ c]  a''[ a]
  a,[ a ]  c[ c]
  c,32[  c]  a''[ a]
  f,[ f]  e'[ e]
  c,64[ c]  a''[ a]
  f,[ f]  e'[ e]
}


