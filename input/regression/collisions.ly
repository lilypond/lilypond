\version "1.3.146"
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
		\stemDown <a4 c> <a4 c> <a4 c> <a4 c> <a c>
		<a c> <a c>  
	}
>


threevoice =  \context Staff \notes <
	\context Voice=i { \stemUp g4 f e f g a g2 }
	\context Voice=ii { \stemUp \shiftOn
		e4 e e e e e e e  }
	\context Voice=iii { \stemDown c4 d e d c d es }
>

hairyChord =  \context Staff \notes\relative c' <
     \context Voice=one {
\shiftOff
	\stemUp 
	e4 
     }
     
     \context Voice=two {
	\stemUp
	\shiftOn
 \property Voice.NoteColumn \override #'horizontal-shift = #1
	cis
     }
     
     \context Voice=three {	\shiftOnn
	\stemUp 
	ais
     }
     
     \context Voice=four {
        \stemDown
	\shiftOn

	fis
     }
>


\score{
	\notes \transpose c'' {
	   \twovoice
	   \twovoicechords
	
	\threevoice 
	 \hairyChord
	}
	
%	\midi { \tempo 4:80 }
}
