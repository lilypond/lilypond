#(ly:set-option 'old-relative)
\version "1.9.2"
\header{
texidoc="
Normal collisions. We have support for polyphony, where the
middle voices are horizontally shifted.
"
filename = 	 "collisions.ly"
enteredby = 	 "HWN,JCN"
copyright = 	 "public domain"
}


twovoice =
\notes \relative c' \context Staff \notes < 
	 {  g4 f f e e d d c } 
\\ {  c4 c  d d e e f f }
>

twovoicechords = \context Staff \notes <
 \relative c' {
 e4 d c b a g f
	}\\
 \relative c' {
	 <<a c>>4 <<a c>>4 <<a c>>4 <<a c>>4 <<a c>>
		<<a c>> <<a c>>  
	}
>


threevoice =  \context Staff \notes <
	 { g4 f e f g a g2 } \\
	 {  c4 d e d c d es } \\
	 { e4 e e e e e e e  }
>

hairyChord =  \context Staff \notes \relative c''
    < e \\
      fis, \\
      cis' \\
      \\
      ais
      >


\score{
	\notes { \transpose c c' {
	   \twovoice
	   \twovoicechords
	   \threevoice   }
	 \hairyChord
	}
	
%	\midi { \tempo 4:80 }
}

