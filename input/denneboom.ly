\header{
filename =	 "denneboom.ly";
enteredby =	 "jcn";
copyright =	 "public domain";
}
%{ file tests silly line shapes %}

\include "paper20.ly"

\version "1.1.52";

oden = \lyrics{ 
	O |
	den- ne- boom, o | 
	den- ne- boom  Wat |
	zijn uw tak- ken | 
	won- der- schoon 
}

ikheb = \lyrics{
	Ik | 
	heb u laatst in_'t |
	bos zien staan  toen |
	zat- en er geen |
	kaars- jes aan 
}

ugloeit = \lyrics{
	U |
	gloeit in bar- re | 
	win- ter- tijd, als |
	sneeuw op aar- de | 
	licht ge- spreid 
}

oboom = \notes\transpose c''{
	g,8 |
	c8. c16 c4. d8 | 
	e8. e16 e4. e8 | 
	d  e f4 b, | 
	d8. c16 c4 r8
}

bos = \notes\transpose c''{
	g8 | 
	g e a4. g8 | 
	g8. f16 f4 r8 f8 |
	f d g4. f8 | 
	f8. e16 e4 r8
}


melody = \notes{
	\oboom
	\oboom
	\bos
	\oboom
}


verse_one = \notes{
	\oden
	\oden
	\ikheb
	\oden
}

verse_one_staff = \context Lyrics = one 
	\verse_one


verse_two = \notes{
	\oden
	\oden
	\ugloeit
	\oden
}

verse_two_staff = \context Lyrics = two 
	\verse_two


denneboom_shape = \paper{ 
	\paper_twenty
	indent = 20. \mm;
	\shape   70. \mm, 20. \mm,
		 65. \mm, 30. \mm,
		 57.5 \mm, 45. \mm,
		 50. \mm, 60. \mm,
		 42.5 \mm, 75. \mm,
		 35. \mm, 90. \mm,
		 27.5 \mm, 105. \mm,
		 20. \mm, 120. \mm,
		 10. \mm, 140. \mm,
		 65. \mm, 30. \mm
		 ;
	gourlay_maxmeasures = 30.;
	arithmetic_basicspace = 3.8;
	arithmetic_multiplier = 8.\pt;
}

\score{
	\addlyrics
		\context Staff { \time 3/4; \melody }
		\context Lyrics \verse_one
	\paper{ 
		\denneboom_shape 
	}
	\midi{ \tempo 4 = 90; }
}
