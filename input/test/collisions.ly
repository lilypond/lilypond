\header{
filename =	 "collisions.ly";
description =	 "random counterpoint to test collisions";
enteredby =	 "HWN,JCN";
copyright =	 "public domain";
Tested =	 "test the Collision resolution ";
}
\version "1.3.96";

twovoice = \context Staff \notes < 
	\context Voice=i { \stemDown c4 d e f g2~  g4 a [c8 d e f] c2| }
	\context Voice=ii { \stemUp g4 f e g ~ g2 g2  c4 g4 g2 } 
>

twovoicesteminvert = \context Staff \notes <  
	% the f and g on 4th beat are exceptionally ugh.
	\context Voice=i { \stemUp c4 d e f g2 g4 a | }
	\context Voice=ii { \stemDown g4 f e g  g2 g2 } 
>

threevoice = \context Staff \notes <
	\context Voice=i { \stemUp g4 f e f g a g2 }
	\context Voice=ii { \stemUp \property Voice.NoteColumn \push #'horizontal-shift = #1
		e2  e2  e2  e2 }
	\context Voice=iii { \stemDown c4 d e d c d es }
>

chordstest = \context Staff \notes <
	\context Voice = i \relative c {
		\stemUp e4 dis c f g f a b b
	}
	\context Voice = ii \relative c {
		\stemDown <a4 c> <a4 c> <a4 e'> <a4 c> <e' a> <e a> <e a> <a c> <a d>
	}
>

hairyChord = \context Staff \notes\relative c' <
     \context Voice=one {
 \property Voice.NoteColumn \push #'horizontal-shift = #0
	\stemUp 
	e4 
     }
     
     \context Voice=two {
	\stemUp
 \property Voice.NoteColumn \push #'horizontal-shift = #1
	cis
     }
     
     \context Voice=three {
 \property Voice.NoteColumn \push #'horizontal-shift = #2

	\stemUp 
	ais
     }
     
     \context Voice=four {
        \stemDown
 \property Voice.NoteColumn \push #'horizontal-shift = #1

	fis
     }
>


\score{
	\notes \transpose c'' {  \twovoice  
	\twovoicesteminvert 
	\threevoice \break
	 \chordstest
	 \hairyChord
	}
	
%	\midi { \tempo 4:80 }
}
