%{MudelaHeader

 filename: scsii-menuetto.ly
 title: Solo Cello Suites, Suite II part V, Menuetto I
 description: Cello suite transcribed for Viola
 composers: Johann Sebastian Bach (1685-1750)
 entered-by: JCN
 copyright: public domain

 Tested Features:breaking algorithm, chords, multivoice, accents


EndMudelaHeader
%}

\version "0.1.0";

%% Stuff from MPP version
% \lefttitle{Menuetto}
% \tempo{Moderato}
% \metron{4=120}
%
% \key\F		% \key is F ( f-major )
%
% \slope{30}		% Fool MusiXTeX into 30 / 10 steeper beam
%		% because piece is set very tightly

IImenuetto = \melodic{
	\clef"alto";
	\meter 3/4;
	\key bes;
	\octave c';

	\duration 8;
		< a2 f2 d2 \f > bes4-.  |
%%2
	< [ bes8^"1" e c_"2"_"4" >
		a bes-. g-. ] a4-.^"4" |
%%3
	< d4 'bes4-. > g4-. [ f8-. e-. ] |
%%4
	< [ f 'a > e d cis-. 'b-. 'a-. ]
%%5
	< a2 f2 d2 > bes!4-. |
%%6
	< [ bes8 e > a bes-. g-. ]
	c'!4-. |
%%7
	< a4-. f4>
	< d'4-.-\upbow f4 'bes4 >
	< e'4-.-\downbow g4 'g4 > |
%%8
	< cis'2.-\upbow e2. 'a2. > 
		\bar ":|:";
%%9
	\clef "violin";
	< e'2 a2 \f >
	[ d'8( e'16 )f'16 ] |
%%10
	< [ e'8 g > d' cis'_"2" e' a~  g ] |
%%11
	< \multi 2;  
		{\stem 1; a4~  d'4 cis'4-. }
		{ \stem -1; f2 e4 } > |
%%12
	< [ g'8^"4" d > f' e' f' d'^"3"~   c' ] |
%%13
	\clef "alto";
	< \multi 2;
		 { \stem 1; bes2 c'4 }
		 { \stem -1; g4~  f4 e4 }% ugh
	> |
%%14
	< [ a8 f > g a f ] < d'4-\upbow e4 'bes4 > |
%%15
	< c'4-\downbow f4 'a4 > [ bes8~  a g a ] |
%%16
	[ f( e )f a-. g-. bes-. ] |
%%17
	< a2^"0" fis2_"3" > bes4 |
%%18
	< { [ c'8~  bes c' a ] } { [ es ] } > fis4 |
%%19
	< d'4-\downbow g4 'bes4 > < c'4-\upbow g4 c4 > < [ bes8 d > a ] |
%%20
	< { [ c'8 ( bes a )bes g~  bes ] } { [ d ] } { [ g ] } > |
%%21
	< \multi 2;  
		{\stem 1;  d'4 (\stem 1; ) cis'4 d'4 }  
		{ \stem -1; g2 f4 } 
	> |
%%22
	< { [ g8 ~  f g e ] } { [ cis ] } > < f4 d4 > |
%%23
	[ 'g8 g ] < e4. 'a4. > d-\upbow |
%%24
	< d2.^"fine" 'a2. 'd2._"3 mins."> 
	\bar ":|";
%% \tighten		% use one line less
	
}

\score{
	\melodic{ 
		\id "Staff" "cello"; \IImenuetto 
	}
	\paper{
		unitspace= 9.0\mm  ;% to get lily to use only three lines
		geometric= 1.40;
		linewidth= 195.0\mm;
		\output "scsii-menuetto.out";
	}
	\midi{ 
		\tempo 4:120;
		\output "scsii-menuetto.midi";
	}
}
