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
	\context GrandStaff <
	\context Staff = SA <
		\context Voice = VA { \property Voice.verticalDirection= 1
			e4 dis4 e2 }
		\context Voice = VB { \property Voice.verticalDirection= -1
			[cis8 a] [fis b] gis2 }
		{\key e; }
		>
	\context Staff = SB { \clef "bass"; \key e;
		[a,,16 e dis e] [b'8 b,] [e16 e, gis b] e4
	} 
> 

\paper 
{
%	linewidth = 5.0 \cm; % ly2dvi barfs on -1
	linewidth = 8.0 \cm;
%	linewidth = 12.0 \cm;	
}
}

\version "1.0.20"; 
