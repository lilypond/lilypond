\header {
opus = "BWV 937 (prelude)";
composer = "J. S. Bach";
source = "Petits Preludes et Fugues.  Urtext. Editions Henry Lemoine, Paris.";
}

%{
1. upon stretching: every note should stretch according to duration

2. 8th notes should be spaced equidistantly.
%}

\score { 
    \notes \relative c''
	\type GrandStaff <
	\type Staff = SA <
		\type Voice = VA { \property Voice.ydirection= 1
			e4 dis4 e2 }
		\type Voice = VB { \property Voice.ydirection= -1
			[cis8 a] [fis b] gis2 }
		{\key e; }
		>
	\type Staff = SB { \clef "bass"; \key e;
		[a,,16 e dis e] [b'8 b,] [e16 e, gis b] e4
	} 
> 

\paper 
{
}
\paper 
{
%	linewidth = 5.0 \cm; % ly2dvi barfs on -1
	linewidth = 8.0 \cm;
}
}
