% sarabande-urtext.ly
% belongs together with -cello.ly and -alto.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging music and scripts doen't work too well yet (see alto_scripts).
%}

n = { \slurnormal }
d = { \slurdotted }

sarabande_a = \melodic{
%	\octave relative;
	\voiceone \d[d,8.()e16] e4.-\trill([d16 )e] |
	f4. \onevoice [e8(d)c] |
	[bes g']\n[f e16(f][g a bes)d,] |
	cis4.-\trill\d[b8(a)g] |
	% copy bar 1, half bar 2
	\voiceone \d[d'8.()e16] e4.-\trill([d16 )e] |
	%5
	f'4. \onevoice [d8(d)e] |
	\n[g bes16()a][c()bes a()g][d'8 f,] |
	\voiceone e4.-\trill \onevoice \d[d8(c)bes] |
	\voiceone \n[f' g16()a] a4. [g16()f] |
	[g8 a16()bes]a4. [c16()d]|
	% 11
	\onevoice \d [e,8()f][c,, g'][f'()e] |
	f4 f,2 |
	\voiceone a''4 \d a4.-\trill()bes8 |
	[c bes16()a] \onevoice [fis8.-\trill()es16][d8()c] |
	[bes g'][a, fis'][es'()d] |
	%16
	\voiceone bes4.-\trill \onevoice [a8(g)f!] |
	[g bes,()a f'(g)a] |
	[d, as,()g es'(f)g]|
	[cis, bes'][a g16()f][e!8 f16()d] |
	[cis8 e16()a][a,8. g'16]\n[f8()e] |
	%21
	\voiceone [d e16()f]f4. [e16()d] |
	[e8 f16()g]g4. [a16()bes] |
	\d[a8 cis16()d][d,8 e16(f32)g][f8-\trill \n e16()d] |
	d4 [d,16 a'( b cis][d e f )g] |
	%25
	[a(b c)b] c4. [b16()a] |
	\d[b(cis d )cis] d4. \n[e16()f] |
	\onevoice [d(cis)d f,] [a,8 e']\d[d'()cis] |
	d4 d,,2 |
	\octave c';
}

sarabande_b = \melodic{
%	\octave relative;
	\voicetwo
% dispute
%	d4 a,2 |
%	this avoids the clash bug
	d8. s16 a,2 |
	<d,4. a'> s8*3 |
	s2.*2
%	d4 a,2 |
	d8. s16 a,2 |
	<d,4. a'> s8*3 |
	s2. |
	<c,4. g'> s8*3  |
	a4 <bes4. d> r8 |
	bes4 <g2 f'> |
	s2.*2 |
	\d[f8()es] e4. r8 |
	d4 s2 |
	s2. |
	<g,4. d'> s8*3  |
	s2.*4 |
	bes4 g2  |
	g4 <cis,4. bes'> s8 |
	<f,8 a d'> r r g, a4 |
	s2. |
	f'4 fis4. s8 |
	<g,4 d'> gis'4. s8 |
	\octave c,;
}


sarabande = \type Voice \melodic<
	\$sarabande_a
	\$sarabande_b
>

