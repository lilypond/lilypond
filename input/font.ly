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
		\bar "|:";
		\meter 4/4;
		\multi 2  < { \stemup e'\longa a\breve | }
		{ \stemdown
		c1 \clef "bass"; ''b \clef "violin"; c' a'
		c2 g c' a'
		} >
		\stemboth
		c4 g c' a' \bar ":|";
		a\ppp-\upbow a\pp-\downbow a\p^\turn a\mp^\fermata |
		a\mf_\fermata a\f-\stopped a\ff-\open a\fff^\trill|
		a\fp a4.\sf a4.\sfz | % a\fz a\rf
		[c8 c] [a' a']
		[c a'] [a' c] |
		[c d e f] [as' ges' f' e']
		[cis' dis' c' des'] [cisis' disis' ceses' deses'] |
		\clef "bass";
		  r1 r2 r4 r8 r16 r32 r64 r128 r128 |
		\multi 2 < { \stemup r2 r2} {\stemdown c c }>
		\multi 2 < { \stemup ''c1 } {\stemdown r1}>		
		\stemboth
		\clef "violin";
		e8_. g'8-> e16^^ g'16_^ 
		e32 _| g'32^| g''32-\ltoe g''32-\lheel
		e64 g'64 c4... |

		\meter 1/2; a2 |
		\meter 3/2; a1. |
		\meter 2/4; a2 |
		\meter 5/4; a1.. |
		\meter 6/8; a2. |
		\meter 7/8; a2.. |
		\meter 9/8; a1... |
		\meter 12/8; a1. |
		}
	\paper{ 
	    % don't change this.
	    % otherwise 16pt and 20pt layouts differ.
	    linewidth = 12.5 \cm;
	    gourlay_maxmeasures =5.;
	}
	\paper{
	    \paper_twenty
	    linewidth = 17.5 \cm;
	    gourlay_maxmeasures =5.;
	    \output "font20.tex";
	}
}

