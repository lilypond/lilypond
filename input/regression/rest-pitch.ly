#(ly:set-option 'old-relative)
\version "2.1.7"
\header {

texidoc = "Rests can have pitches--these will be affected by
transposition and relativization. If a rest has a pitch, rest
collision will leave it alone."

}

    \paper { raggedright= ##t }

\score { \notes\relative c'' 
{
   a4\rest b4\rest c4\rest

<<d \\  d\rest^"rest pitch" >>
<<d \\  r>>
  c16 [ d r e] 
  c16 [ d e\rest^"rest pitch" e] 
  
}
}

