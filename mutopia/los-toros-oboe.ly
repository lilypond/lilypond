\header{
filename =	 "los-toros-oboe.ly";
title =	         "Los Toros";
opus =	         "";
composer =	 "Paul Lac\^ome d'Estalenx (1838-1920)";
enteredby =	 "jcn";
copyright =	 "public domain";
} 


hobo1 = \melodic{
	\octave c';
	[es'16-. es'-. es'-. es'-.] [es'8-. d'-.] |
	[f'8.-> es'16(] [)d'8 c'-.] |
	[bes16( c' d' es'] [)d'8 c'-.] |
	[bes-. as->~] [as16( g f g] |
	[as bes c' d'] [)es'8 c'-.] |
	[d'8-. c'16( bes] )a4 ~ |
	[as16 g f g] [as c' bes as] |
%	[)g8 as16(] [)f8 g16( f] |
	[)g8. as16(] [)f8 g16( f] |
	[)es8 f16-. g-.] [as-. bes-. c'-. d'-.] |
	[es'-. es'-. es'-. es'-.] [es'8-. d'-.] |
	[f'8.-> es'16(] [)d'8 c'-.] |
	[bes16( c' d' es'] [)d'8 c'-.] |
	[bes8 a->~] [a16 g( fis g] |
	[a bes c' d'] [)es'8 d'16()c'] |
	[bes-. g-. bes-. d'-.] g'4-> ~ |
	[g'16 f'( es' d'] [c' es'  d' c'] |
	[)bes8 c'16( bes] [)a8 bes16( a] |
	[)g8  r d'] r |
	[g16-. g-. g-. g-.] [g8-. f-.] |
	as2-> |
	[as16-. as-. as-. as-.] [as8-. g-.] |
	bes2-> |
	[bes16-. bes-. bes-. bes-.] [bes8-. c'] |
	r-"cresc." [d'8-. r c'-.] |
	r\f [d'-. es'-. f'-.] |
	[g'8.-> es'16] [bes8 g] |
	[g16-.\p g-. g-. g-.] [g8-. f-.] |
	as2-> |
	[as16-. as-. as-. as-.] [as8-. g-.] |
	bes2-> |
	[b16-. b-. b-. b-.] [b8-. c'-.] |
	[b16-. b-. b-. b-.] [b8-. c'-.] |
	[b8-.-"cresc." c'-. b-. c'-.] |
	[d'-. es'-. d'-. es'-.] |
	[f'\f-. g'-. f'-. g'-.] |
	[as'-.\< g'-. as'-. bes'-.] |
	[\!g'16\ff-. g'-. g'-. g'-.] [g'8-. g'-.] |
	[g'8.-> g'16] [g'8 g'] |
	[g'8.-> g'16] [g'8 g'] |
	[f'8.-> f'16] [f'8 f'] |
	[f'8.-> f'16] [f'8 f'] |
	[bes'8-. as'16( g'] )f'4 ~ |
	[f'16 es'( d' es'] [f' as' g' )f'] |
	[es'( g' f' es'] [d' g' es' d'] |
	[c' es' d' c'] [b d' c' b] |
	[a c' bes a] [g bes a g] |
	[fis a g fis] [e g fis )e] |
	% a 2
}

global = \melodic{
	\key bes es as;
	\meter 2/4;
}

$staff_hobo1 = \type Staff = hobo1 <
	\global
	\hobo1
>

\score{
	\$staff_hobo1
	\paper{}
	\midi{}
}
