\header{
filename =	 "collisions.ly";
description =	 "random counterpoint to test collisions";
enteredby =	 "HWN,JCN";
copyright =	 "public domain";
Tested =	 "test the Collision resolution ";
}
\version "1.0.21";

twovoice = \context Staff \notes < 
	\context Voice=i { \stemdown c4 d e f g2~  g4 a [c8 d e f] c2| }
	\context Voice=ii { \stemup g4 f e g ~ g2 g2  c4 g4 g2 } 
>

twovoicesteminvert = \context Staff \notes <  
	% the f and g on 4th beat are exceptionally ugh.
	\context Voice=i { \stemup c4 d e f g2 g4 a | }
	\context Voice=ii { \stemdown g4 f e g  g2 g2 } 
>

threevoice = \context Staff \notes <
	\context Voice=i { \stemup g4 f e f g a g2 }
	\context Voice=ii { \stemup \property Voice.horizontalNoteShift = 1 e2  e2  e2  e2 }
	\context Voice=iii { \stemdown c4 d e d c d es }
>

\score{
	\notes \transpose c'' {  \twovoice  
	\twovoicesteminvert 
	\threevoice  

	}
	

	
%	\midi { \tempo 4:80 }
}
