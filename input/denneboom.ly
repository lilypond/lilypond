\header{
filename =	 "denneboom.ly";
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures = "This file tests silly line shapes";
}

\version "0.1.8";

boom = \lyric{
	\meter 1/4;
	O4 den- ne- boom, o den- ne- boom.
	Wat zijn uw tak- ken won- der- schoon
	O den- ne- boom, o den- ne- boom.
	Wat zijn uw tak- ken won- der- schoon
	Ik heb u laatst in 't bos zien staan
	toen zat- en er geen kaarsjes aan.
	O, Den- ne- boom, o den- ne- boom.
	Wat zijn uw tak- ken wonder- schoon

	O den- ne- boom, o den- ne- boom.
	Wat zijn uw tak- ken won- der- schoon
	O den- ne- boom, o den- ne- boom.
	Wat zijn uw tak- ken won- der- schoon
	U gloeit in bar- re winter- tijd, 
% ugh
%	als sneeuw op aar- de licht ge- spreid.
%	O, Den- ne- boom, o den- ne- boom.
%	Wat zijn uw tak- ken wonder- schoon
}

ugh = \melodic{
	\octave c';
	\meter 1/4;
	c d e f | c d e f | c d e f | c d e f |
	c d e f | c d e f | c d e f | c d e f |
	c d e f | c d e f | c d e f | c d e f |
	c d e f | c d e f | c d e f | c d e f |
	c d e f | c d e f | c d e f | c d e f |
	c d e f | c d e f | c d e f | c d e f |
	c d e f | c d e f | c d e
}

\score{
%	<
%		\type Lyrics \boom
		\ugh
%	>
	\paper{
		\indent = 20. \mm;
		\shape = 72.5 \mm 15. \mm
			 65. \mm 30. \mm
			 57.5 \mm 45. \mm
			 50. \mm 60. \mm
			 42.5 \mm 75. \mm
			 35. \mm 90. \mm
			 27.5 \mm 105. \mm
			 20. \mm 120. \mm
			 10. \mm 140. \mm
			  0. \mm 160. \mm
			 72.5 \mm 15. \mm
%			 72.5 \mm 15. \mm
			 60. \mm 40. \mm
			 ;

		gourlay_maxmeasures = 30.;
	}
}
