\header {
texidoc = "The staff is a grob, and may be adjusted as well: this one
 shows a staff with 6 thick line, and a slightly large staffspace.
Beams remain correctly quantized.

"    
}

\score { \notes {
    \property Score.StaffSymbol \set #'thickness = #2.0
    \property Score.StaffSymbol \set #'line-count = #6
    \property Score.StaffSymbol \set #'staff-space = #1.1
    c'4 g'4 b'8  b'8  b'8  b'8  
}
	 
\paper  { linewidth = -1.


      } 
     }
