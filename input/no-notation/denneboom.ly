\version "2.7.39"
\header{
filename = 	 "denneboom.ly"
enteredby = 	 "jcn"
copyright = 	 "public domain"
}
%{ file tests silly line shapes %}

#(set-global-staff-size 20)



oden = \lyricmode{ 
	O |
	den- ne- boom, o | 
	den- ne- boom  Wat |
	zijn uw tak- ken | 
	won- der- schoon 
}

ikheb = \lyricmode{
	Ik | 
	heb u laatst in_'t |
	bos zien staan  toen |
	zat- en er geen |
	kaars- jes aan 
}

ugloeit = \lyricmode{
	U |
	gloeit in bar- re | 
	win- ter- tijd, als |
	sneeuw op aar- de | 
	licht ge- spreid 
}

oboom = \transpose c c'{
	g,8 |
	c8. c16 c4. d8 | 
	e8. e16 e4. e8 | 
	d  e f4 b, | 
	d8. c16 c4 r8
}

bos = \transpose c c'{
	g8 | 
	g e a4. g8 | 
	g8. f16 f4 r8 f8 |
	f d g4. f8 | 
	f8. e16 e4 r8
}


melody = {
	\oboom
	\oboom
	\bos
	\oboom
}


verseOne = {
	\oden
	\oden
	\ikheb
	\oden
}

verseOneStaff = \new Lyrics =  "one" 
	\verseOne


verseTwo = {
	\oden
	\oden
	\ugloeit
	\oden
}

verseTwoStaff = \new Lyrics =  "two" 
	\verseTwo


denneboomShape = \layout{ 
	\layoutTwenty
	indent = 20. \mm

	% UGH -- THIS IS HAIRY 
	#'margin-shape = #(map
		(lambda (x) (cons-map mm-to-pt x)) 
		'((70.0 .  20.)
		 (65.0 .  30.0)
		 (57.5  .  45.0)
		 (50.0 .  60.0) 
		 (42.5  .  75.)  
		 (35.0 .  90.)
		 (27.5  .  105.)
		 (20.0 .  120.0)
		 (10.0 .  140.0) 
		 (65.0 .  30.0))
	)

	gourlay_maxmeasures = 30.
	arithmetic_basicspace = 3.8
	arithmetic_multiplier = 8.\pt
}

\score{
	\oldaddlyrics
		\context Staff { \time 3/4 \melody }
		\context Lyrics \verseOne
	\layout{ 
		\denneboomShape 
	}
	\midi{ \tempo 4 = 90 }
}
