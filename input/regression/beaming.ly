
\version "2.19.21"

\header{
texidoc="
Beaming is generated automatically. Beams may cross bar lines. In that
case, line breaks are forbidden.
"
}

\context Staff  \relative  {

  c''8[ \tuplet 3/2 { c16 d e] }
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

  c8[^"over bar line" c c] c8
  c16[ c8 c16]
  c32[ c16 c16 c16 c32]
  c32[ c16 c8 c32]		 % hmm ?
  
}

