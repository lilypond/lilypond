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
 Tested Features:breaking algorithm, chords, multivoice, accents, 
 dotted slurs
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

%{
  This file has a long heritage.
  It should probably be rewritten as two separate voices.
%}


IImenuetto = \melodic{
	\clef"alto";
	\property Staff.instrument = cello
	\meter 3/4;
	\key bes;
	\octave c';

	<a2 f d \f> bes4-.  |
	<{\voiceone; [bes8^1 a bes-. g-.] a4-.^4 } {\voicetwo; <e8_2 c_4> }> |
	\onevoice;
	<d4 'bes-.> g-. [f8-. e-.] |
	<{\voiceone; [f8( e )d cis-. 'b-. 'a-.] } {\voicetwo; 'a8 }> |
	\onevoice;
	<a2 f d> bes!4-. |
	<{\voiceone; [bes8 a bes-. g-.] } {\voicetwo; e8 }>
	\onevoice;
	c'!4-. | <a-. f> <d'-.-\upbow f 'bes>
	<e'-.-\downbow g 'g> | <cis'2.-\upbow e 'a> \bar ":|:";
	\clef "violin";
	<e'2 a\f>
	[d'8( e'16 )f'] |
	\slurdotted;
	<{\voiceone; [e'8( d' cis'_2 )e' a( )g] } {\voicetwo; g8 }> |
	\onevoice;
	<{\voiceone; a4 ~ d' cis'-. } {\voicetwo; f2 e4 }> |
	\onevoice;
	<{\voiceone; [g'8^4( f' e' )f' d'^3( ) c'] } {\voicetwo; d8 }> |
	\onevoice;
	\clef "alto";
	<{\voiceone; bes2 c'4 } {\voicetwo; g4 ~ f e }> |
	\onevoice;
	<{\voiceone; [a8 g a f] } {\voicetwo; f8 }>
	\onevoice;
	<d'4-\upbow e 'bes> |
	<c'-\downbow f 'a> [bes8 a g a] |
	[f8( e )f a-. g-. bes-.] |
	<a2^"0"^\trill fis_3> bes4 |
	<{\voiceone; [c'8 bes c' a] } {\voicetwo; es8 }>
	\onevoice;
	fis4^\trill |
	<d'4-\downbow g 'bes> <c'-\upbow g c> <[bes8( d> )a] |
	<{\voiceone; [c'8( bes a )bes g( )bes] } {\voicetwo; <d8 g> }> |
	\onevoice;
	<{\voiceone; d'4 ~ cis' d' }  {\voicetwo; g2 f4 }> |
	\onevoice;
	<{\voiceone; [g8 f g e] f4 } {\voicetwo; cis8 d4 }> |
	\onevoice;
	['g8 g] <'a4. { e^\trill ~ d8-\upbow }> |
	\textstyle "italic";		% ugh
	<d2._"fine" 'a 'd> \bar ":|";
}

\score{
	\melodic{ 
		\IImenuetto 
	}
	\paper{
		linewidth= 185.0\mm;
	}
	\midi{ 
		\tempo 4 = 120;
	}
}
