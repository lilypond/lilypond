\version "1.3.93";

FontBody=	\notes\transpose c''{ 
		\bar "|:";
		\time 4/4;
		\context Staff  < \context Voice = VB { \stemUp e'\longa a\breve | }
		\context Voice = VA { \stemDown
		c1 \clef "bass"; b,, \clef "violin"; c' a'
		c2 \clef "alto"; g c' \clef "violin"; a'
		} >
		\stemBoth
		c4 g c' a' \bar ":|";
		a\ppp-\upbow a\pp-\downbow a\p^\turn a\mp^\fermata |
		a\mf_\fermata a\f-\stopped a\ff-\open a\fff^\trill|
		a\fp-\reverseturn a4.\sf a4.\sfz |  a4\fz % a\rf
		[c8-\prall c-\mordent] [a'-\prallmordent a'-\prallprall]
		[c8-\upprall a'8-\downprall] [a'-\segno c-\coda] |
		[c \< d e f] [as' ges' f' e']
		[cis' dis' c' des'] [cisis' disis' \! ceses' deses'] |
		\clef "bass";
		  r\longa * 1/4 r\breve *1/2
		  r1 r2 r4 r8 r16 r32 r64 r128 r128 |
		\context Staff < \context Voice = VA { \stemUp r2 c'2 c,,,1 }
				\context Voice = VB {\stemDown r2 c2  r1 }>
			\stemBoth
		\clef "violin";
		e8_. g'8-> e16^^ g'16_^ 
		e32 _| g'32^| g''32-\ltoe g''32-\lheel
		e64-\rtoe g'64-\rheel c4... |
\context Voice = mensural \relative c'' {
\property Voice . noteHeadStyle = #'mensural
\property Voice. stemCentered = ##t
c\maxima*1/8
c\longa*1/4 c\breve*1/2 c1 c2 c4 c8 
}
			\property Voice . noteHeadStyle = #'harmonic

		\transpose c'{
			\time 4/4;
			\property Voice . textStyle =  "finger"
			\property Voice . noteHeadStyle = #'diamond
			c1^"1" d2^"2" e4^"3"
			\property Voice . noteHeadStyle = #'cross
						 f4^"4"
						  g4^"5"
			\property Voice . noteHeadStyle = ##f
			% Music to the Martians!
			< bes4^"6" e_"7" c_"8" >
			a^"0"_"9"

			a'^\flageolet
			\property Voice . textStyle =  "roman"
			\time 1/2; a2 |
			\time 3/2; < a1.
			{ s4 \ppp \< s4 \! s4 \fff  s4 \> s4 \! s4\ppp} >
			|
			\time 2/4; a2 |
			\time 5/4; a1.. |
			\time 6/8; a2. |
			\time 7/8; a2.. |
			\time 9/8; a1... |
			\time 12/8; a1. |
			\time 12/4;
			r1. r2. r4. r8. r16. r32. r64. r64. |
			c2\sustaindown c\sustainup |
		}
}
