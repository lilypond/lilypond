\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests the Feta music-font"
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "0.1.7";

\score{
	\melodic{ 
		\octave c';
		\meter 4/4;
		\multi 2  < { \stemup e'\longa a\breve | }
		{ \stemdown
		c1 g c' a'
		c2 g c' a'
		} >
		\stemboth
		c4 g c' a' |
		a\ppp-\upbow a\pp-\downbow a\p^\turn a\mp^\fermata |
		a\mf_\fermata a\f-\stopped a\ff-\open a\fff^\trill|
		a\fp a4.\sf a4.\sfz | % a\fz a\rf
		[c8 c] [a' a']
		[c a'] [a' c] |
		[c d e f] [as' ges' f' e']
		[cis' dis' c' des'] [cisis' disis' ceses' deses'] |
		  r1 r2 r4 r8 r16 r32 r64 r128 r128 |
		\multi 2 < { \stemup r1} {\stemdown c''}>
		\multi 2 < { \stemup c1 } {\stemdown r1}>		
		\stemboth
		
		c8_. c''8-> c16^^ c16_^ 
		c32 _| c32^| g''32-\ltoe g''32-\lheel
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

