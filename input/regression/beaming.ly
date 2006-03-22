
\version "2.8.0"

\header{
texidoc="
Beaming is generated automatically. Beams may cross bar lines. In that
case, line breaks are forbidden.  Yet clef and key signatures are
hidden just as with breakable bar lines.
"
}

\context Staff  \relative c''  {
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

  c8[ c c] c8 % over barline
  c16[ c8 c16]
  c32[ c16 c16 c16 c32]
  c32[ c16 c8 c32]		 % hmm ?
  
}

