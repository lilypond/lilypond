\header{
filename =	 "opus-130.ly";
title =	 "Quartett";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
opus = "130";
enteredby =	 "JCN";
copyright =	 "public domain";
}

% \;

tempi = \notes {
	\property Voice.TextScript \set #'font-style = #'large
	s8^"Allegro assai"
}

dynamics = \notes {
	s8\p\< \!s8.\> \!s16 | s4.\p | s8\< s8. \!s16 | s4.\p |
	s8\p\< \!s8.\> \!s16 | s4.\p | s8\< s8. \!s16 | s4.\p |
}

global = \notes {
	\key g \major;
	\time 3/8;
	< 
	        \dynamics
		s4. *  8
		s4. *  8
		% \repeat semi 2 { s4.*8 } 
	>
}

violinei = \notes\relative c''{
	d8(b)d16 r | g,4. | a16(b c8)e16 r | g,8~fis4 | 
	d''8(b)d16 r | g,4. | a16(b c8)fis,16 r | fis8~g4
}

violineii = \notes\relative c'{
	b8(d)b | e(g,)e' | e(c)a' | a(c)a | 
	% copy from violinei: 5-8
	d8(b)d16 r | g,4. | a16( b c8)fis,16 r | fis8~g4
}

viola = \notes\relative c'{
	\clef "alto";
	g8(b)g | b(e,)b' | c,(a')c, | c'(d,)c' | b(d)b | 
	e(g,)e' | e(e,<)a' c,> | < a(c,> <fis b,> )b, 
}

cello = \notes\relative c'{
	\clef "bass";
	g4 r8 | e'4 r8 | c4 r8 | d4 r8 | g,,8 b g | b(e,)b' |
	c,(a')d, | d'(d,)g 
}

\score{
	\context StaffGroup <
% broken, 1.1.23
%		\context Staff = i < \tempi \global \violinei >
		\context Staff = i < \global \violinei >
		\context Staff = ii < \global \violineii >
		\context Staff = iii < \global \viola >
		\context Staff = iv  < \global \cello >
	>
	\paper{
		\translator { \OrchestralScoreContext }
	}
	\midi{ \tempo 4 = 160; }
}

