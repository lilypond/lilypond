\header{
filename =	 "denneboom.ly";
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures = "This file tests silly line shapes";
}

\include "paper20.ly"

\version "1.0.7";

oden = \lyrics{ 
	O8 |
	den-8. ne-16 boom,4. o8 | 
	den-8. ne-16 boom.4.  Wat8 |
	zijn uw tak-4 ken | 
	won-8. der-16 schoon4 _8
}

ikheb = \lyrics{
	Ik8 | 
	heb u laatst4. in_'t8 |
	bos8. zien16 staan4 _8 toen8 |
	zat- en er4. geen8 |
	kaars-8. jes16 aan.4 _8
}

ugloeit = \lyrics{
	U8 |
	gloeit in bar-4. re8 | 
	win-8. ter-16 tijd,4 r8 als8 |
	sneeuw op aar-4. de8 | 
	licht8. ge-16 spreid.4 _8
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

global = \notes{ 
	\time 3/4;
    	\partial 8;
	\skip 4*48;
	\bar "|.";
}
melody = \notes{
	\oboom
	\oboom
	\bos
	\oboom
}

$melody_staff = \type Staff = melody <
	\global
	\melody
>

$verse_one = \notes{
	\oden
	\oden
	\ikheb
	\oden
}

$verse_one_staff = \type Lyrics = one <
	\global
	\$verse_one
>

$verse_two = \notes{
	\oden
	\oden
	\ugloeit
	\oden
}

$verse_two_staff = \type Lyrics = two <
	\global
	\$verse_two
>

$denneboom_shape = \paper{ 
	\paper_twenty
	indent = 20. \mm;
	\shape = 70. \mm 20. \mm
		 65. \mm 30. \mm
		 57.5 \mm 45. \mm
		 50. \mm 60. \mm
		 42.5 \mm 75. \mm
		 35. \mm 90. \mm
		 27.5 \mm 105. \mm
		 20. \mm 120. \mm
		 10. \mm 140. \mm
		 65. \mm 30. \mm
		 ;
	gourlay_maxmeasures = 30.;
% uhuh, loop if you comment these in
%		arithmetic_basicspace = 3.8;
%		arithmetic_multiplier = 8.\pt;
}

\score{
	<
		\$melody_staff
		\$verse_one_staff
	>
	\paper{ 
% huh?
%		\$denneboom_shape 
	\paper_twenty
	indent = 20. \mm;
	\shape = 70. \mm 50. \mm
		 65. \mm 30. \mm
		 57.5 \mm 45. \mm
		 50. \mm 60. \mm
		 42.5 \mm 75. \mm
		 35. \mm 90. \mm
		 27.5 \mm 105. \mm
		 20. \mm 120. \mm
		 10. \mm 140. \mm
%		 65. \mm 30. \mm
		 65. \mm 40. \mm
		 ;
	gourlay_maxmeasures = 30.;
% uhuh, loop if you comment these in
		arithmetic_basicspace = 3.8;
%		arithmetic_multiplier = 8.\pt;
		arithmetic_multiplier = 7.5\pt;
	}
	\midi{ \tempo 4 = 90; }
}

%{
\score{
	<
		\$melody_staff
		\$verse_two_staff
	>
	\paper{ \$denneboom_shape }
	\midi{ \tempo 4 = 90; }
}
%}
