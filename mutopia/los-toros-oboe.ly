\header{
filename =	 "los-toros-oboe.ly";
title =	         "Los Toros";
opus =	         "";
composer =	 "Paul Lac\^ome d'Estalenx (1838-1920)";
enteredby =	 "jcn";
copyright =	 "public domain";
} 

\include "paper20.ly"

hoboonestart = \melodic{
% ugh: can't copy: allegro/primo tempo
	\octave c';
	\textstyle "large";
	[es'16-.^"Allegro" es'-. es'-. es'-.] [es'8-. d'-.] |
	\textstyle "italic";
}

hobooneintro = \melodic{
	\octave c';
%	\textstyle "roman";
%	[es'16-.-"Allegro" es'-. es'-. es'-.] [es'8-. d'-.] |
%	\textstyle "italic";
	[f'8.-> es'16(] [)d'8 c'-.] |
	[bes16( c' d' es'] [)d'8 c'-.] |
	[bes-. as->~] [as16( g f g] |
	[as bes c' d'] [)es'8 c'-.] |
	[d'8-. c'16( bes] )as4 ~ |
	[as16 g( f g] [as c' bes as] |
%	[)g8 as16 g(] [)f8 g16( f] |
	[)g8 as16 g(] [)f8 g16( f] |
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
}

hoboonemid = \melodic{
	\octave c';
	[g16-. g-. g-. g-.] [g8-. f-.] |
	as2-> |
	[as16-. as-. as-. as-.] [as8-. g-.] |
	bes2-> |
	[bes16-. bes-. bes-. bes-.] [bes8-. c'] |

	r-"cresc." [d'8-. r c'-.] |
	r\f [d'-. es'-. f'-.] |
	[g'8.-> es'16] [bes8 g] |

	% four measures copied from 8 measures back...
	[g16-.\p g-. g-. g-.] [g8-. f-.] |
	as2-> |
	[as16-. as-. as-. as-.] [as8-. g-.] |
	bes2-> |
	[b16-. b-. b-. b-.] [b8-. c'-.] |

	% same measure
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
	[es'( g' f' es'] [d' f' es' d'] |
	[c' es' d' c'] [b d' c' b] |
	[a c' bes a] [g bes a g] |
	[fis a g fis] [e g fis )e] |
	% `a deux
	[d8 d'] [es'8.-> c'16] |
	[a8 bes g es] |
	[c' d' es'8. bes16] |
	[g8 es bes c] |
	[d16-. d-. d-. d-.] [d8.-. es16->] |
	[d'16-.-"cresc." d'-. d'-. d'-.] [d'8.-. es'16->] |
	[d'16-. d'-. d'-. d'-.] [d'8.-. es'16->] |
	[d'16 d' d' d'] [d'8 es'16-.] r16-\fermata^"court"^"tr\\`es" |
	% Un peu plus lent.
	\textstyle "bold";

% ugh
%	d'4(\<^"Un peu plus lent" \textstyle "italic; _"tr\\`es \\'el\\'egant"
%	d'4(\^"Un peu plus lent et \\'el\\'egant"<
	\textstyle "italic"; 
	d'4(\<
	[e'8 \!f'8] |
%	[)b'-. a'-.] [g16( fis' e' )d'] |
	\textstyle "bold";
	[)b'-.^"Un peu plus lent et \\'el\\'egant" a'-.] [g16( fis' e' )d'] |
	\textstyle "italic"; 
	[d'( c' b\< c'] [)\!f'8-. e'-.] |
	d4-> ~ [d16 e( fis g] |
	[a b\< d' fis] [)\!a'8-. g'-.] |
	[fis'16( e' a )c'] e'4 ~ |
	[e'16 d'( e c'] [)b8-. a-.] |
	g2 ~ |
	[g8 \[/3 d16( e fis ]1/1 [)g8 d'-.] |
	b2-> ~ |
	[b8 \[/3 d16( e fis ]1/1 [)g8-"cresc. poco" e'-.] |
	e'2-> ~ |
	[e'8 \[/3 d16( e fis ]1/1 [)g8\f d'-.] |
	[d'8.-> b'16-.] [g'16-. d'-. b-. c'-.] |
	[d'-. e'-. fis'-. a'-.] [g'8-. e'-.] |
	fis'2-> ~ |
	[fis'16 g'-.\< a'-. b'-.] [\!c''8-. c'-.] |
	[es'8.->(-"espress.") d'16] d'4 ~ |
	[d'16 e'( fis a'] [)g'8 b-.] |
	[b8.->( )c'16] c4 ~ |
	[c'16\< cis'( d' \!dis'] [)e'8-.-"dim." fis-.] |
	[fis8.->( g16] )g4 ~ |

	% (only notes! of) five measures copied from 14 measures above
	[g8 \[/3 d16( e fis ]1/1 [)g8 d'-.] |
	b2->-"cresc." ~ |
	[b8 \[/3 d16( e fis ]1/1 [)g8 e'-.] |
	e'2-> ~ |
	[e'8 \[/3 d16(_"h\\^atez" e fis ]1/1 [)g8\f d'-.] |
	[d'8.-> b16-.] [g16-.-"cresc." d'-. b-. c'-.] |
	[d'16\f\< e'-. fis'-. g'-.] [a'-. b-. c'-. \!d'-.] 
}


hoboonesecondstart = \melodic{
	\octave c';
% ugh: can't copy: allegro/primo tempo
	\textstyle "large";
	[es'16-.\ff^"Tempo 1$^o$" es'-. es'-. es'-.] [es'8-. d'-.] |
	\textstyle "italic";
	
}
	
hoboonelast = \melodic{
	\octave c';
	% could transpose/copy from measure 19...
	[d'16-.\p  d'-. d'-. d'-.] [d'8-. c'-.] |
	es'2-> |
	[es'16-. es'-. es'-. es'-.] [es'8-. d'-.] |
	f'2-> |
	[f'16-. f'-. f'-. f'-.] [f'8-. g'-.] |
	r8 [a'-.-"cresc." r g'-.] |
	r [a'-.\f bes'-.\< \!c''-.] |
	[d''8.->\> \!bes'16] [f'8 d'] |

	% four measures copied from 8 measures back...
	[d'16-.\p  d'-. d'-. d'-.] [d'8-. c'-.] |
	es'2-> |
	[es'16-. es'-. es'-. es'-.] [es'8-. d'-.-"cresc."] |
	f'2-> |
	[fis'16-.\p  fis'-. fis'-. fis'-.] [fis'8-. g'-.] |
	% same measure
	[fis'16-.\p  fis'-. fis'-. fis'-.] [fis'8-. g'-.] |
	[fis'8-. g-. fis-. g-.] |
	[a'-. bes'-. a'-. bes'-. ] |
	[a'-.\f bes'-. a'-. bes'-. ] |
	[c''-.-"cresc." f'-. g'-. a'-.] |
	[bes'-. bes'-. ces''8.-> as'16] |
	[f'8-. ges'-. es'-. ces'-.] |
	% `a deux
	[as-. bes-. ces'8.-> ges16] |
	[es8-. ces'-. ges-. as-.] |
	bes\p r r4 |
	[bes16-.-"cresc." bes-. bes-. bes-.] [bes8-. ces'->] |
	% same measure
	[bes16-. bes-. bes-. bes-.] [bes8-. ces'->] |
	[bes'16-.\ff bes'-. bes'-. bes'-.] [bes'8-. ces''16->] r16^"court"-\fermata |
	% ugh: eight measures rest (ugh: r1 -> four beats...)
	\textstyle "bold";
%	r2^"Un peu plus lent et \\'el\\'egant"
	r2
	r4 r8\p bes |
	g2->^"Un peu plus lent et \\'el\\'egant" ~ |
	\textstyle "italic";
	g8 r r c' |
	c'2-> ~ |
	c'8 r r bes'\f |
	[bes'8.->\f g'16-.] [es'16-. bes-. g-. as-.] |
	[bes-.-"dim." c'-. d'-. f'-.] [es'8-. c'-.] |
	d'2-> ~ |
	[d'16-"dim."( es' f' g'] [)as'8 as-.] |
	[ces'8.->\p( )bes16-.] bes4 ~ |
	[bes16( c' d' f] [)es'8 g-.] |
	[g8.->( )as16] as4 ~ |
	[as16( a bes b] [)c'8-. d-.] |
	% a deux
	[d8.->( ) es16] es4 ~ |
	es4 r8 bes-. |
	g2-> ~ |
	g8 r r c' |
	c'2 ~ |
	c'8  r r bes' |
	[bes'8.-> g'16-.] [es'16-. bes-. g-. as-.] |
	[bes-. c'-. d'-. es'-.] [f'-. g'-. as'-. bes'-.] |
	d''8-. r c''4-> ~ |
	[c''16 f-. g'-. a-.] [bes'-. c'-. d'-. es'-.] |
	g'8-. r f'4-> ~ |
	[f'16 bes-. c'-. d-.] [es'-. f'-. g-. as-.] |
	c''8 r bes'4 ~ |
	[bes'8 as'-. g'8.-. e'16-.] |
	g'8-. r f4-> ~ |
	[f'8 es'-. ces'8.-. as16-.] |
	% `a deux
	\textstyle "large";
	[ces'8.->^"Plus vite" bes16-.(] [a bes es' d'] |
	\textstyle "italic";
	[)c!8.-> bes16] [a( bes es' )d'] |
	[c'->( bes es' )d'] [c'->( bes es' )d'] |
	% same measure
	[c'->( bes es' )d'] [c'->( bes es' )d'] |
	[c'( bes a bes] [c' d es' e] |
	[g' )f' d'( es'] [f' g' as' a'] |
	[c'' )bes' bes( c'] [d' es' f' g'] |
	[as' g' f' g'] [as' bes' c'' d''] |
	\textstyle "large";
	[)es''-.^"Tempo 1$^o$" g'-. g'-. g-.] [g'8-. g'-.] |
	\textstyle "italic";
	bes'4.-> g'8-. |
	gis'2->( |
	)as'! |
	[g'16-. g'-. g'-. g-.] [g'8-. g'-.] |
	bes'4.-> g'8-. |
	gis'2->( |
%	)as'! |
	)as' |
	[bes'16-. bes'-. bes'-. bes-.] [bes'8-. bes'-.] |
	bes'4.-> g'8-. |
	[g'16-._"h\\^atez" g'-. g'-. g-.] [g'8-. g'-.] |
	g'4.-> g'8-. |
	es'4.-> es'8-. |
	bes4.-> bes8-. |
	\textstyle "large";
	g'4.->^"Presto" g'8-. |
	\textstyle "italic";
	es'4.-> es'8-. |
	[bes-. bes-. es'-. g-.] |
	[bes'-. bes-. es'-. g'-.] |
	bes'-. r r4 |
	g'8-. r r4 |
	g'8-. r r4 |
}

hoboone = \melodic {
	\hoboonestart
	\hobooneintro
	\hoboonemid
	\hoboonesecondstart
	\hobooneintro
	\hoboonelast
}

global = \melodic{
	\key bes es as;
	\meter 2/4;
	\skip 4*110;
	\key fis; |
	\bar "||";
	\skip 4*58;
	\key bes es as; |
	\bar "||";
	\skip 4*206;
	\bar "|.";
}


$staff_hoboone = \type Staff = hoboonestaff <
	\global
	\property Staff.instrument = "oboe"
	\hoboone
>

a4 = \paper{
	\paper_twenty
	linewidth= 165.\mm;
	gourlay_maxmeasures = 10.0;
	Score = \translator {
		\type Score_engraver;

		\consists "Timing_engraver";
		\consists "Bar_column_engraver";
		\consists "Bar_number_engraver";

		\consists "Span_score_bar_engraver";
		\consists "Score_priority_engraver";
		\consists "Priority_horizontal_align_engraver";
		\consists "Vertical_align_engraver";


		\accepts "Staff_group";
		\accepts "Staff";
		\accepts "Rhythmic_staff";	
		\accepts "Lyrics";
		\accepts "Grandstaff";
	}
}

a4sixteen = \paper{
	linewidth= 165.\mm;
	Score = \translator {
		\type Score_engraver;

		\consists "Timing_engraver";
		\consists "Bar_column_engraver";
		\consists "Bar_number_engraver";

		\consists "Span_score_bar_engraver";
		\consists "Score_priority_engraver";
		\consists "Priority_horizontal_align_engraver";
		\consists "Vertical_align_engraver";


		\accepts "Staff_group";
		\accepts "Staff";
		\accepts "Rhythmic_staff";	
		\accepts "Lyrics";
		\accepts "Grandstaff";
	}
}

\score{
	\$staff_hoboone
	\paper{ \a4 }
%	\paper{ \a4sixteen }
	\midi{
		\tempo 4 = 80;
	}
}

