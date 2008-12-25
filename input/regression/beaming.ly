
\version "2.12.0"

\header{
texidoc="
Beaming is generated automatically. Beams may cross bar lines. In that
case, line breaks are forbidden.
"
}

\context Staff  \relative c''  {

  c8[ \times 2/3 { c16 d e] }
  s4*3
  
  c8.[ c16]
  c8.[ c16 c8.  c16]
  c16[ c8.] |
  c8.[ c16  c16  c8.]
  c8.[ c32  c32]
  c8[ c8] |
  c16[ c16]
  c32[ c32]
  c64[ c64]	
  c32
  c2

  c8[^"over barline" c c] c8 
  c16[ c8 c16]
  c32[ c16 c16 c16 c32]
  c32[ c16 c8 c32]		 % hmm ?
  
}

