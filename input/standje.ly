\header{
filename =	 "standchen.ly";
title =	 "St\"andchen (Serenade) \"Leise flehen meine Lieder\"";
opus =	 "D. 957 No. 4";
composer =	 "Franz Schubert (1797-1828)"
	 "Text by Ludwig Rellstab (1799-1860)";
enteredby =	 "JCN";
copyright =	 "public domain";
} 

%{
 Tested Features: multivoice, accents, lyrics, chords, piano music,
multiple \paper{}s in one \score 
%}

\version "0.1.7";

$vocal_verse1 = \melodic{
	\octave c';
	% ugh: treble/bass
%	\clef treble;
	\clef violin;
	% ugh: '3' of plet should hang over middle note
	% ugh: barcheck fails if i write 'g8' below:
%	[2/3 g8( )as]1/1 \plet 2/3; g8 \plet 1/1; c'4. g8 |
	% anyway, for typping this plet stuff sucks
	[2/3 g8( )as]1/1 \plet 2/3; g \plet 1/1; c'4. g8 |
	[2/3 f8( )g]1/1 \plet 2/3; f \plet 1/1; c'4 f8 r |
	g4.-> f8 [2/3 f( )es]1/1 \plet 2/3; d \plet 1/1; |
	es2 r4 |
	% ugh: a whole should be a measure
	%r1 |
	r2. |
	r2. |
	[2/3 g8( )as]1/1 \plet 2/3; g \plet 1/1; es'4. g8 |
	[2/3 f8( )g]1/1 \plet 2/3; f \plet 1/1; d'4. c'8 |
	bes4. as8 [2/3 as( )g]1/1 \plet 2/3; f \plet 1/1; |
	g2 r4 |
	r2. |
	r2. |
	g8. b16 es'4. d'8 |
	c'8. g16 es4. c8 |
	%  [as32\grace( bes32\grace] )
	[2/3 as8( )g]1/1 \plet 2/3; as \plet 1/1; c'4. as8 |
	g2. |
	%[f32\grace( g] )
	[2/3 f8( )e]1/1 \plet 2/3; f \plet 1/1; as4. f8 |
	es!2. |
	g8. b16 es'4. d'8 |
	c'8. g16 e4. c8 |
	% [a32\grace b] )
	[2/3 a!8( ) gis]1/1 \plet 2/3; a \plet 1/1; c'4. a8 |
	g!2. |
	[2/3 d'8\f cis']1/1 \plet 2/3; d \plet 1/1; f'4. b8 |
	c'2. |
	r2. |
	r2. |
}

$lyric_verse1 = \lyric{
% 5
	[2/3 Lei-4 se8 ]1/1 fleh-4. en8  |
	[2/3 mei-4 ne8 ]1/1 Lie-4 der8 _8 |
	Durch4. die8 [2/3 Nacht4 zu8 ]1/1  |
	dir;2 _4 |
	_4 _ _ |
	_ _ _ |
% 11
	[2/3 In4 den8 ]1/1 stil-4. len8 |
	[2/3 Hainr4 her-8 ]1/1 nie-4. der,8 |
	Lieb4. chen,8 [2/3 komm4 zu8 ]1/1 |
	mir!2 _4 |
	_4 _ _  |
	_ _ _ |
% 17
	Fl\"us-8. ternd16 schlan-4. ke8 |
	Wip-8. fel16 rau-4. schen8 |
	[2/3 In4 des8 ]1/1 Mon-4. des8 |
	Licht;2. |
	_4 _ _  |
	_4 _ _ |
% 23
	Des8. Ver-16 r\"a-4. ters8 |
	feind-8. lich16 Lau-4. schen8 |
	[2/3 F\"urch-4 te,8 ]1/1 Hol-4. de,8 |
	nicht,2. |
	[2/3 f\"urch-4 te,8 ]1/1 Hol-4. de,8 |
	nicht.2. |
	_4 _ _ 
	_ _ _
}
	
$treble_intro = \melodic{
	\octave c';
	\clef violin;
	% ugh: i'd like to type this!
        %r8 [<'g-. c-.> <c-. es-.> <'g-. c-.> <c-. es-.> <'g-. c-.>] |
        r8 <['g-. c-.> <c-. es-.> <'g-. c-.> <c-. es-.> <'g-. c-.]> |
	r8 <['as-. c-.> <c-. es-.> <'as-. c-.> <c-. es-.> <'as-. c-.]> |
	r8 <['as-. c-.> <c-. d-.> <'as-. c-.> <c-. d-.> <'as-. c-.]> |
	r8 <['g-. 'b-.> <'b-. d-.> <'g-. 'b-.> <'b-. d-.> <'g-. 'b-.]> |
	
}

$treble_verse1 = \melodic{
	\octave c';
	\clef violin;
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['f c> <c d> <'f c> <c d> <'f c]> |
	r8 <['f 'g b> <'g b d> <'f 'g b> <'g b d> <'f 'g b]> |
	r8 <['es 'g c> <'g c es> <'es 'g c> <'g c es> <'es 'g c]> |
	<g'4.( b> <)f'8 d'> <[2/3 f' d'> <es' c'> <d' b]1/1> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
	r8 <['g c> <c es> <'g c> <c es> <'g c]> |
}

$bass_intro = \melodic{
	\octave c;
	\clef bass;
	<'c2 c> r4 |
	<''as2 'as> r4 |
	<''f2 'f> r4 |
	<''g2 g> r4 |
}

$bass_verse1 = \melodic{
	\octave c;
	\clef bass;
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
	<'c2 c> r4 |
}
		
		
global= \melodic {\meter 3/4; \key bes es as; }

		
\score{
	\melodic<
		\type Lyrics { \skip 4 * 12; \$lyric_verse1 }
		\type Staff 
		{ \skip 4 * 12; \$vocal_verse1 }
		
		% i want a (grand) staff!!!
		
		\type Grandstaff < 
			< { \$treble_intro \$treble_verse1 }
			  \global 
			>
			< \global
			  { \$bass_intro \$bass_verse1 }
			>
		>
	>
	\paper{
		% how does this work?
		% previously we had the clear 'ideal whole note space' setting?
		arithmetic_multiplier = 6.\pt;
		% ugh: the *first* (intro) line may only have four measures...
		gourlay_maxmeasures = 4.;
	}
}

