\header{
filename =	 "denneboom.ly";
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures = "This file tests silly line shapes";
}

\include "paper20.ly"

\version "0.1.9";

oden = \lyric{ 
	_8 O8 |
	den-8. ne-16 boom,4 _8 o8 | 
	den-8. ne-16 boom.4.  Wat8 zijn uw | 
	tak-4 ken won-8. der-16 schoon4
}

ikheb = \lyric{
	_8 Ik8 heb u |
	laatst4. in_'t8 bos8. zien16 staan4.  toen8 zat- en |
	er4. geen8 kaars-8. jes16 aan.4
}

ugloeit = \lyric{
	_8 U8 gloeit in |
	bar-4. re8 win-8. ter-16 tijd,4.  als8 sneeuw op |
	aar-4. de8 licht8. ge-16 spreid.4
}

oboom = \melodic{
	\octave c';
	r8 'g8 |
	c8. c16 c4 r8 d8 | 
	e8. e16 e4 r8 e8 d  e  | 
	f4 'b d8. c16 c4
}

bos = \melodic{
	\octave c';
	r8 g8 g e |
	a4 r8 g8 g8. f16 f4 r8 f8 f d |
	g4 r8 f8 f8. e16 e4 
}

global = \melodic{ 
	\meter 3/4;
    	\partial 4;
	\skip 4*1;
	\skip 4*3;
	\meter 4/4;
	\skip 4*4;
	\meter 5/4;
	\skip 4*5;
	\meter 3/4;
	\skip 4*3;
	\meter 4/4;
	\skip 4*4;
	\meter 6/4;
	\skip 4*6;
	\skip 4*6;
	\meter 5/4;
	\skip 4*5;
	\meter 3/4;
	\skip 4*3;
	\meter 4/4;
	\skip 4*8;
	\bar "|.";
}

\score{
	<
		\lyric \type Lyrics <
			\multi 2 < 
% huh?
%				\global
				{
					\oden
					\oden
					\ikheb
					\oden
				}
			>
		>
		\melodic \type Staff < 
			\multi 2 < 
				\global
				{
					\oboom
					\oboom
					\bos
					\oboom
				}
			>
		> 
	>
	\paper{
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
	\midi{
		\tempo 4 = 90;
	}
}

%{
\score{
	<
		\lyric \type Lyrics <
			\multi 2 < 
% huh?
%				\global
				{
					\oden
					\oden
					\ugloeit
					\oden
				}
			>
		>
		\melodic \type Staff < 
			\multi 2 < 
				\global
				{
					\oboom
					\oboom
					\bos
					\oboom
				}
			>
		> 
	>
	\paper{
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
%			  0. \mm 160. \mm
			 65. \mm 30. \mm
			 ;
		gourlay_maxmeasures = 30.;
% uhuh, loop if you comment these in
%		arithmetic_basicspace = 3.8;
%		arithmetic_multiplier = 8.\pt;
	}
	\midi{
		\tempo 4 = 90;
	}
}
%}
