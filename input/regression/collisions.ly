\version "1.7.16"
\header{
texidoc="
Normal collisions. We have support for polyphony, where the
middle voices are horizontally shifted.
"
filename = 	 "collisions.ly"
description = 	 "random counterpoint to test collisions"
enteredby = 	 "HWN,JCN"
copyright = 	 "public domain"
Tested = 	 "test the Collision resolution "
}


twovoice =
\notes \relative c' \context Staff \notes < 
	\context Voice=i { \stemDown c4 c  d d e e f f }
	\context Voice=ii { \stemUp g4 f f e e d d c } 
>

twovoicechords = \context Staff \notes <
	\context Voice = i \relative c' {
		\stemUp e4 d c b a g f
	}
	\context Voice = ii \relative c' {
		\stemDown <<a c>>4 <<a c>>4 <<a c>>4 <<a c>>4 <<a c>>
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
    i>


\score{
	\notes { \transpose c c' {
	   \twovoice
	   \twovoicechords
	   \threevoice   }
	 \hairyChord
	}
	
%	\midi { \tempo 4:80 }
}
%% new-chords-done %%
