\header{
filename =	"twinkle.ly";
title =		"Twinkle Twinkle Little Star";
composer =	"Traditional";
enteredby =	"hwn and jcn";
copyright =	"public domain";
}

\version "1.3.25";

\include "paper-as5.ly"

melody = \notes \relative c'' {
	\time 2/4;
	c4 c | g' g | a a | g g |
%{
	f f | e e | d d8.( e16 | )c2 |

	g'4 g | f f | e e | d d |
	g g | f f | e( e8. f16 | e4 )d |

	c c | g' g | a a | g g |
	f f | e e | d d8.( e16 | )c2 |
%}
}

accompany = \notes \relative c {
	\clef "bass";
	\time 2/4;
	c4 c' | e c | f c | e c | 
%{
	d b | c a | f g | c,2 | 

	e'4 g, | d' g, | c g | b g | 
	e' g, | d' g, | c c8.( d16 | c4 )b |

	c, c' | e c | f c | e c | 
	d b | c a | f g | c,2 
%}
}


text = \lyrics{
	Twin -- kle, twin -- kle, lit -- tle star, " "
%{
	How I won -- der what you  " " are.
	Up a -- bove the world so high, " "
	Like a dia -- mond in the  " " sky. " "
	Twin -- kle, twin -- kle, lit -- tle star, " "
	How I won -- der what you  " " are!
%}
}

\score{
	\context GrandStaff <
		\addlyrics
			\context Staff=upper \melody
		\context Lyrics=between \text
		\context Staff=lower \accompany 
	>
	\paper{
		\translator { \GrandStaffContext \accepts "Lyrics"; }
		indent=4.0\char;
		linewidth=78.0\char;
	}
}

