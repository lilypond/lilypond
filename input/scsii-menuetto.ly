% scsii-menuetto.ly
%
% Solo Cello Suites
% J. S. Bach
%
% Suite II part v
% Menuetto I
%
% Copyright (c) 1995,1996,1997 Jan Nieuwenhuizen <jan@digicash.com>
% 
% The purpose of this file is to demonstrate features of LilyPond.
% (there is an accompanying LaTeX file, scsii-menuetto.tex)
%
\version "0.0.52";
%% Stuff from MPP version
% \lefttitle{Menuetto}
% \tempo{Moderato}
% \metron{4=120}
%
% \key\F		% \key is F ( f-major )
%
% \slope{30}		% Fool MusiXTeX into 30 / 10 steeper beam
% \stemlength3		% shorter \stemlength
%		% because piece is set very tightly

ii_menuetto = \melodic{
	\clef"alto";
	\meter 3/4;
	\key bes;
	\octave c';

	\duration 8;
		< a2 f2 d2 \f > bes4-.  |
%%2
	< [ bes^"1" e c_"2"_"4" >
		a bes-. g-. ] a4-.^"4" |
%%3
	< d4 'bes4-. > g4-. [ f-. e-. ] |
%%4
	< [ f 'a > e d cis-. 'b-. 'a-. ]
%%5
	< a2 f2 d2 > bes!4-. |
%%6
	< [ bes e > a bes-. g-. ]
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
	[ d'( e'16 )f'16 ] |
%%10
	< [ e' g > d' cis'_"2" e' a( )g ] |
%%11
	< \multivoice  
		{\stem 1; a4() d'4 cis'4-. }
		{ \stem -1; f2 e4 } > |
%%12
	< [ g'^"4" d > f' e' f' d'^"3"( ) c' ] |
%%13
	\clef "alto";
	< \multivoice
		 { \stem 1; bes2 c'4 }
		 { \stem -1; g4() f4 e4 }% ugh
	> |
%%14
	< [ a f > g a f ] < d'4-\upbow e4 'bes4 > |
%%15
	< c'4-\downbow f4 'a4 > [ bes( )a g a ] |
%%16
	[ f( e )f a-. g-. bes-. ] |
%%17
	< a2^"0" fis2_"3" > bes4 |
%%18
	< { [ c'( )bes c' a ] } { [ es ] } > fis4 |
%%19
	< d'4-\downbow g4 'bes4 > < c'4-\upbow g4 c4 > < [ bes d > a ] |
%%20
	< { [ c'( bes a )bes g( )bes ] } { [ d ] } { [ g ] } > |
%%21
	< \multivoice  {\stem 1;  d'4(\stem 1; ) cis'4 d'4 }  { \stem -1; g2 f4 } > |
%%22
	< { [ g( )f g e ] } { [ cis ] } > < f4 d4 > |
%%23
	[ 'g g ] < e4. 'a4. > d-\upbow |
%%24
	< d2.^"fine" 'a2. 'd2._"3 mins."> 
	\bar ":|";
%% \tighten		% use one line less
	
}

\score{
	\staff{ melodicregs ii_menuetto }
	\paper{
		\width 195\mm
		\unitspace 9\mm  % to get lily to use only three lines
		\geometric 1.4
		\output "scsii-menuetto.out"
	}
	\midi{ 
		\tempo 4:120
		\output "scsii-menuetto.midi"
	}
}
