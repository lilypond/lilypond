\version "0.1.14";
FontBody=	\melodic{ 
		\octave c';
		\bar "|:";
		\meter 4/4;
		\multi 2  < { \stemup e'\longa a\breve | }
		{ \stemdown
		c1 \clef "bass"; b,, \clef "violin"; c' a'
		c2 \clef "alto"; g c' \clef "violin"; a'
		} >
		\stemboth
		c4 g c' a' \bar ":|";
		a\ppp-\upbow a\pp-\downbow a\p^\turn a\mp^\fermata |
		a\mf_\fermata a\f-\stopped a\ff-\open a\fff^\trill|
		a\fp-\reverseturn a4.\sf a4.\sfz | % a\fz a\rf
		[c8-\prall c-\mordent] [a'-\prallmordent a'-\prallprall]
		[c-\upprall a'-\downprall] [a' c] |
		[c \< d e f] [as' ges' f' e']
		[cis' dis' c' des'] [cisis' disis' \! ceses' deses'] |
		\clef "bass";
		  r1 r2 r4 r8 r16 r32 r64 r128 r128 |
		\multi 2 < { \stemup r2 r2} {\stemdown c c }>
		\multi 2 < { \stemup c,,1 } {\stemdown r1}>		
		\stemboth
		\clef "violin";
		e8_. g'8-> e16^^ g'16_^ 
		e32 _| g'32^| g''32-\ltoe g''32-\lheel
		e64-\rtoe g'64-\rheel c4... |

		\octave c'';
		\meter 4/4;
		\textstyle "finger";
		c4^"1" d^"2" e^"3" f^"4" g^"5"
		
		% Music to the Martians!
		< bes4^"6" e_"7" c_"8" >
		a^"0"_"9"
		
		a'^\flageolet
		\textstyle "roman";
		\meter 1/2; a2 |
		\meter 3/2; < a1.
		{ s4 \ppp \< s4 \! s4 \fff  s4 \> s4 \! s4\ppp} >
		|
		\meter 2/4; a2 |
		\meter 5/4; a1.. |
		\meter 6/8; a2. |
		\meter 7/8; a2.. |
		\meter 9/8; a1... |
		\meter 12/8; a1. |
		\meter 12/4;
		r1. r2. r4. r8. r16. r32. r64. r64. |
		}
