\header{
filename =	 "toccata-fuga-E.ly";
title =	 "toccata and fuga in E-major ";
opus =	 "BWV 566";
description = "toccata 3 bar excerpt, "
	 "2nd fuga	transposed subject --- 4 bar excerpt";

composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "JCN";
copyright =	 "public domain";

}

%{
 Tested Features:
 purpose of this file is testing: 
     * real-life collisions
     * multi-voice input --- splitting?
     * organ staff...
%}


\version "0.1.8";

toccata_commands = \melodic{
	\meter 4/4;
	\key fis cis gis dis; % E-major
}

toccata_right = \melodic{ 
	\$toccata_commands 
	% 13 -- how to type -- where to split -- this more neatly ?
	\octave c';
	\multi 2 <
	 	{ \stemup r4 dis'4 e'4.-. e'8( | \property Voice.hshift = 1 )e'4 [d'8 fis'8] \property Voice.hshift = 0 gis'4 [gis'8 fis'16 e'16] | fis'4~  [fis'8 e'16 dis'16] e'4 r8 e'8 }
		{ \stemup \property Voice.hshift = 1 r4 bis4 cis'4-. cis'4 | \property Voice.hshift = 0 a'4~  [a'16 gis'16 a'16 b'16] \property Voice.hshift = 1 dis'4 cis'4~  [cis'8 dis'16 a16] bis4 cis'4 r8 b8 }
		{ \stemdown r4 < gis4 fis4 dis4 > gis4-. gis4 | a4. cis'8 gis2 | fis4 gis4 gis4 r8 e8 }
	> |
	% 16
}

toccata_left = \melodic{ 
	\$toccata_commands  
	\octave c;
	\clef bass;
	% 13
	\multi 2 <
		{ \stemup bis2 cis'4-. cis'4~  [cis'8 a8 d'8 cis'8] [bis8 gis8] cis'4 | dis'2 cis'4 r8 cis'8 }
		{ \stemup \property Voice.hshift = 1 r4 gis4. gis8~  gis4 | \stemdown \property Voice.hshift = 0 a4. fis8 gis4. a8~  a4 gis4-. gis4 r8 gis8 }
		{ \stemdown r4 < dis4 fis4 > < cis4 e4 > }
	> |
	% 16
}

toccata_pedal = \melodic{
	\$toccata_commands  
	\octave c;
	\clef bass;
	% 13
	r4 'fis4-\ltoe 'e4.-\lheel e8-\rheel | 
	% 14
	fis4.-\rtoe fis8-.-\rtoe fis4-\rtoe [e8-\ltoe a8-\rtoe] | 
	% 15
	dis4-\ltoe gis4-\rtoe [cis8-\ltoe 'b8-\lheel 'a8-\rtoe 'gis8-\ltoe] |
	% 16
}

fuga2_commands = \melodic{
	\meter 3/4;
	\key fis cis gis dis; % E-major
}

fuga2_right = \melodic{
	\$fuga2_commands  
	\clef violin;
	% 15
	\octave c';
	\multi 2 <
		{ \stemup [b8 fis8] b4 }
		{ \stemdown fis2 }
	>
	%{ this chord is usually set like this:
	     |
            x|| 
             x||
              x|
           |x
           |
	%}
	\multi 2 <
		{ \stemup \property Voice.hshift = 0 e'4 }
		{ \stemup \property Voice.hshift = 1 cis'4 }
		{ \stemup \property Voice.hshift = 2 ais4 }
		{ \stemdown fis4 }
	> |
	% 16
	\multi 2 <
		{ \stemup dis'2 dis'4 | cis'2 cis'4 | b4~  [b8 cis'8 dis'8 e'8] }
		{ \stemup \property Voice.hshift = 1 [b8 fis8] b2~  [b8 a16 g16] a2 | a4 gis2 }
		{ \stemdown fis2.~  fis2.~  fis4 e2 }
	> |
	% 19
}

fuga2_left = \melodic{
	\$fuga2_commands  
	\octave c;
	\clef bass;
	% 15
	b2 
	\multi 2 < 
		{ \stemup ais4 | bes2. }
		{ \stemdown e4  | fis2 fis4 }
	>
	% 17
	cis'2 e'4 |
	% 18
	b4. b8 b4 |
}

fuga2_pedal = \melodic{
	\$fuga2_commands  
	\octave c;
	\clef bass;
	% 15
	dis4.-\ltoe e8-\rtoe cis4 |
	% 16
	'b4.-\lheel [c8-\ltoe dis8-\rtoe e8-\rheel] |
	% 17
	fis4.-\rtoe [e8-\rheel dis8-\rtoe cis8-\ltoe] |
	% 18
	dis4-\rtoe e4-\rheel 'e4-\ltoe |
}

break = \melodic{ 
	%\meter 4/4;
	r1
}

% these should be two separate scores...
\score{
	 \type Score <
		\type Grandstaff <
		   \type Staff=treble {
		   	 \toccata_right \break \fuga2_right 
		   }
		   \type Staff=bass { 
		   	\toccata_left \break \fuga2_left 
		   }
		> 
		\type Staff=pedal {  \toccata_pedal \break \fuga2_pedal }
	>
	\paper{}
	\midi{
		\tempo 4 = 96;
	}
}

