\header{
filename =	 "scsii-menuetto.ly";
title =	 "Cello Suite II, Menuetto I";
subtitle = "Part V";
instrument = "Menuetto I";		% duh
description =	 "Cello suite transcribed for Viola";
source =	 "?";
opus =	 "BWV 1008 no. 5";
composer =	 "Johann Sebastian Bach(1685-1750)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

%{
 Tested Features:breaking algorithm, chords, multivoice, accents
%}

\version "0.1.10";

%% Stuff from MPP version
% \lefttitle{Menuetto}
% \tempo{Moderato}
% \metron{4=120}
%
% \key\F		% \key is F( f-major )
%
% \slope{30}		% Fool MusiXTeX into 30 / 10 steeper beam
%		% because piece is set very tightly

IImenuetto = \melodic{
%	\property Voice.beamslopedamping = \infinity
	\clef"alto";
	\property Staff.instrument = cello
	\meter 3/4;
	\key bes;
	\octave c';

	\duration 8;
		< a2 f2 d2 \f > bes4-.  |
%%2
	\textstyle "finger";		% ugh.
	
	\multi 2 < 
		{ \stemup; [ bes8^"1"( )a8 bes8-. g8-. ] a4-.^"4" }
		
		{ \stemdown; < e8_"2" c8_"4" > }
	> |
	\stemboth;
%%3
%	\property Voice.beamslopedamping = \normal
	< d4 'bes4-. > g4-. [ f8-. e8-. ] |
%	\property Voice.beamslopedamping = \infinity
%%4
	\multi 2 < 
		{ \stemup; [ f8( e8 )d8 cis8-. 'b8-. 'a8-. ] }
		{ \stemdown; 'a }
	> |
	\stemboth;
%%5
	< a2 f2 d2 > bes!4-. |
%%6
	\multi 2 < 
		{ \stemup; [ bes8 a8 bes8-. g8-. ] }
		{ \stemdown; e8 }
	>
	\stemboth;
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
%	\property Voice.beamslopedamping = \normal
	[ d'8( e'16 )f'16 ] |
%	\property Voice.beamslopedamping = \infinity
%%10
	\multi 2 < 
		{ \stemup; [ e'8( d' cis'_"2" )e' a( )g8 ] }
		{ \stemdown; g8 }
	> |
	\stemboth;
%%11
	\multi 2 <
		{ \stemup; a4( )d'4 cis'4-. }
		{ \stemdown; f2 e4 } 
	> |
	\stemboth;
%%12
	\multi 2 < 
		{ \stemup; [ g'8^"4"( f' e' )f' d'^"3"( ) c' ] } 
		{ \stemdown; d8 }
	> |
	\stemboth;
%%13
	\clef "alto";
	\multi 2 <
		
		{ \stemup; bes2 c'4 }
		{ \stemdown; g4( )f4 e4 }% ugh
	> |
	\stemboth;
%%14
	\multi 2 < 
		{ \stemup; [ a8 g8 a8 f8 ] } 
		{ \stemdown; f8 }
	>
	\stemboth;
	< d'4-\upbow e4 'bes4 > |
%%15
	< c'4-\downbow f4 'a4 > [ bes8( )a8 g8 a8 ] |
%%16
	[ f( e8 )f a8-. g8-. bes8-. ] |
%%17
	< a2^"0"^\trill fis2_"3" > bes4 |
%%18
	\multi 2 < 
		{ \stemup; [ c'8( )bes8 c' a8 ] } 
		{ \stemdown; es8 }
	>
	\stemboth;
	fis4^\trill |
%%19
	< d'4-\downbow g4 'bes4 > < c'4-\upbow g4 c4 > < [ bes8 d8 > a8 ] |
%%20
	\multi 2 < 
		{ \stemup; [ c'8( bes8 a8 )bes g( )bes8 ] }
		{ \stemdown; < d8  g8  > }
	> |
	\stemboth;
%%21
	\multi 2 <  
		{ \stemup; d'4( )cis'4 d'4 }  
		{ \stemdown; g2 f4 }
	> |
	\stemboth;
%%22
	\multi 2 < 
		{ \stemup; [ g8( )f8 g8 e8 ] f4 }
		{ \stemdown; cis8 d4 }
	> |
	\stemboth;
%%23
%	\property Voice.beamslopedamping = \normal
	[ 'g8 g8 ] < e4.^\trill 'a4. > d8-\upbow |
%	\property Voice.beamslopedamping = \infinity
%%24
	\textstyle "roman";		% ugh
	< d2.^"fine" 'a2. 'd2._"3 mins."> 
	\bar ":|";
}

\score{
	\melodic{ 
		\IImenuetto 
	}
	\paper{
		linewidth= 150.0\mm;
		% how does this work?
		% why does space not have dim?
		arithmetic_basicspace = 3.2;
		% how can multiplier have dim?
		arithmetic_mulitplier = 9.\pt;
		%\output "scsii-menuetto.out";
	}
	\midi{ 
		\tempo 4 = 120;
		%\output "scsii-menuetto.midi";
	}
}
