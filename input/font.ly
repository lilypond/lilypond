\header{
enteredby	jcn
copyright	PD
TestedFeatures	font-en-tja
}

\version "0.1.6";

\score{
	\melodic{ 
		\octave c';
		\meter 4/4;
		a\longa
		a\breve |
		c1 g c' a'
		c2 g c' a'
		c4 g c' a' |
		a\ppp a\pp a\p a\mp |
		a\mf a\f a\ff a\fff|
		a\fp a\sf a\sfz a | % a\fz a\rf
		[c8 c] [a' a']
		[c a'] [a' c] |
		[c d e f] [as' ges' f' e']
		[cis' dis' c' des'] [cisis' disis' ceses' deses'] |
		  r1 r2 r4 r8 r16 r32 r64 r128 r128 |
		  c'1.^\fermata c'1._\fermata
		\multi 2 < { \stemup r1} {\stemdown c''}>
		\multi 2 < { \stemup c1 } {\stemdown r1}>		
		\stemboth
		
		c4_. c4-> c4^^ c4_^ 
		c4 _| c4^|
		}
	\paper{ 
	    gourlay_maxmeasures =5.;
	}
	\paper{ 
	    \paper_twenty
	    gourlay_maxmeasures =5.;
	    \output "lelie20.tex";
	}
% oeps
	\midi{ }
}

