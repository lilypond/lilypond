% Creator: GNU LilyPond
% Automatically generated.
% from input file: /home/fred/lelie/current/test/test.midi

\version "0.1.9";
$Track_0 = \melodic{
	% midi copyright:
	% instrument:
	\duration 4;
	% Creator: GNU LilyPond

	 % Automatically generated.

	 % from musical definition: : 1
	 % Track 0
	 \tempo 4=60;
	 
} % Track 0

$track1 = \melodic{
	% midi copyright:
	% instrument:piano
	\duration 4;

	% piano
	 \tempo 4=60;
	 c4  d4  e4  f4  |
	% 2
	g4  a4  b4  c'4  

} % track1

\score{
	\multi 3 < \type Staff
			< \melodic{ \$track1 } >
	>
	\paper{}
	\midi{
		\tempo 4=60;
		 }
}
