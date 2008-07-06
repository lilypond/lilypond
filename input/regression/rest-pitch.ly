\version "2.11.51"
\header {

  texidoc = "Rests can have pitches--these will be affected by
transposition and relativization. If a rest has a pitch, rest/rest and
beam/rest collision resolving will leave it alone."

}

\layout { ragged-right= ##t }

\relative c'' 
{
  a4\rest b4\rest c4\rest

  <<d \\  d\rest^"rest pitch" >>
  <<d \\  r>>
  c16 [ d r e] 
  c16 [ d e\rest^"rest pitch" e] 
  
}


