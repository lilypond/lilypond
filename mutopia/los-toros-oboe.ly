\header{
filename =	 "los-toros-oboe.ly";
title =	         "La Feria";
subtitle = "Los Toros";
opus =	         "";
composer =	 "Paul Lac\\^ome dEstalenx (1838-1920)";
enteredby =	 "jcn";
copyright =	 "public domain";
latexheaders=    "headers";
} 
\version "1.0.7";

%{
Silly latex file dropped; use ly2dvi

Converted to relative octave from los-toros-oboe.ly:
    :s/[^\\]'/'x/g
    :s/'x//g
    (511 substitutions on 155 lines)

    lilypond -f los-toros-oboe 2> bla
    wc -l bla
    138
    138 / 3 =  46 octave quotes of 511 remain!

%}

\include "paper16.ly"

hoboonestart = \notes
  \relative c'{
% ugh: cant copy: allegro/primo tempo
	\property Voice . textstyle =  "large"
	[es''16-.^"Allegro" es-. es-. es-.] [es8-. d-.] |
	\property Voice . textstyle =  "italic"
}

hobooneintro = \notes  \relative c'{

%	\property Voice . textstyle =  "roman"
%	[es''16-.-"Allegro" es-. es-. es-.] [es8-. d-.] |
%	\property Voice . textstyle =  "italic"
	[f''8.-> es16(] [)d8 c-.] |
	[bes16( c d es] [)d8 c-.] |
	[bes-. as->~] [as16( g f g] |
	[as bes c d] [)es8 c-.] |
	[d8-. c16( bes] )as4 ~ |
	[as16 g( f g] [as c bes as] |
%	[)g8 as16 g(] [)f8 g16( f] |
	[)g8 as16 g(] [)f8 g16( f] |
	[)es8 f16-. g-.] [as-. bes-. c-. d-.] |
	[es-. es-. es-. es-.] [es8-. d-.] |
	[f8.-> es16(] [)d8 c-.] |
	[bes16( c d es] [)d8 c-.] |
	[bes8 a->~] [a16 g( fis g] |
	[a bes c d] [)es8 d16()c] |
	[bes-. g-. bes-. d-.] g4-> ~ |
	[g16 f( es d] [c es  d c] |
	[)bes8 c16( bes] [)a8 bes16( a] |
	[)g8  r d'] r |
}

hoboonemid = \notes	\relative c'{

	[g'16-. g-. g-. g-.] [g8-. f-.] |
	as2-> |
	[as16-. as-. as-. as-.] [as8-. g-.] |
	bes2-> |
	[bes16-. bes-. bes-. bes-.] [bes8-. c] |

	r-"cresc." [d8-. r c-.] |
	r\f [d-. es-. f-.] |
	[g8.-> es16] [bes8 g] |

	% four measures copied from 8 measures back...
	[g16-.\p g-. g-. g-.] [g8-. f-.] |
	as2-> |
	[as16-. as-. as-. as-.] [as8-. g-.] |
	bes2-> |
	[b16-. b-. b-. b-.] [b8-. c-.] |

	% same measure
	[b16-. b-. b-. b-.] [b8-. c-.] |

	[b8-.-"cresc." c-. b-. c-.] |
	[d-. es-. d-. es-.] |
	[f\f-. g-. f-. g-.] |
	[as-.\< g-. as-. bes-.] |
	[\!g16\ff-. g-. g-. g-.] [g8-. g-.] |
	[g8.-> g16] [g8 g] |
	[g8.-> g16] [g8 g] |
	[f8.-> f16] [f8 f] |
	[f8.-> f16] [f8 f] |
	[bes8-. as16( g] )f4 ~ |
	[f16 es( d es] [f as g )f] |
	[es( g f es] [d f es d] |
	[c es d c] [b d c b] |
	[a c bes a] [g bes a g] |
	[fis a g fis] [e g fis )e] |
	% `a deux
	[d8 d'] [es8.-> c16] |
	[a8 bes g es] |
	[c' d es8. bes16] |
	[g8 es bes' c,] |
	[d16-. d-. d-. d-.] [d8.-. es16->] |
	[d'16-.-"cresc." d-. d-. d-.] [d8.-. es16->] |
	[d16-. d-. d-. d-.] [d8.-. es16->] |
	[d16 d d d] [d8 es16-.] r16-\fermata^"court"^"tr\\`es" |
	% Un peu plus lent.
	\property Voice . textstyle =  "bold"

% ugh
%	d4(\<^"Un peu plus lent" \property Voice . textstyle =  "italic _"tr\\`es \\'el\\'egant"
%	d4(\^"Un peu plus lent et \\'el\\'egant"<
	\property Voice . textstyle =  "italic" 
	d4(\<
	[e8 \!fis8] |
%	[)b-. a-.] [g16( fis e )d] |
	\property Voice . textstyle =  "bold"
	[)b-.^"Un peu plus lent et \\'el\\'egant" a-.] [g,,16( fis' e )d] |
	\property Voice . textstyle =  "italic" 
	[d( c b\< c] [)\!fis'8-. e-.] |
	d,,4-> ~ [d16 e( fis g] |
	[a b\< d fis] [)\!a8-. g-.] |
	[fis16( e a, )c] e4 ~ |
	[e16 d( e, c'] [)b8-. a-.] |
	g2 ~ |
	[g8 \[/3 d16( e fis ]1/1 [)g8 d'-.] |
	b2-> ~ |
	[b8 \[/3 d,16( e fis ]1/1 [)g8-"cresc. poco" e'-.] |
	e2-> ~ |
	[e8 \[/3 d,,16( e fis ]1/1 [)g8\f d'-.] |
	[d8.-> b'16-.] [g16-. d-. b-. c-.] |
	[d-. e-. fis-. a-.] [g8-. e-.] |
	fis2-> ~ |
	[fis16 g-.\< a-. b-.] [\!c8-. c,-.] |
	[es8.->(-"espress.") d16] d4 ~ |
	[d16 e( fis a] [)g8 b,-.] |
	[b8.->( )c16] c4 ~ |
	[c16\< cis( d \!dis] [)e8-.-"dim." fis,-.] |
	[fis8.->( g16] )g4 ~ |

	% (only notes! of) five measures copied from 14 measures above
	[g8 \[/3 d16( e fis ]1/1 [)g8 d'-.] |
	b2->-"cresc." ~ |
	[b8 \[/3 d,16( e fis ]1/1 [)g8 e'-.] |
	e2-> ~ |
	[e8 \[/3 d,,16(_"h\\^atez" e fis ]1/1 [)g8\f d'-.] |
	[d8.-> b16-.] [g16-.-"cresc." d'-. b-. c-.] |
	[d16\f\< e-. fis-. g-.] [a-. b,-. c-. \!d-.] 
}


hoboonesecondstart = \notes   \relative c'{

% ugh: cant copy: allegro/primo tempo
	\property Voice . textstyle =  "large"
	[es''16-.\ff^"Tempo 1$^o$" es-. es-. es-.] [es8-. d-.] |
	\property Voice . textstyle =  "italic"
	
}
	
hoboonelast = \notes   \relative c'{
	% could transpose/copy from measure 19...
	[d''16-.\p  d-. d-. d-.] [d8-. c-.] |
	es2-> |
	[es16-. es-. es-. es-.] [es8-. d-.] |
	f2-> |
	[f16-. f-. f-. f-.] [f8-. g-.] |
	r8 [a-.-"cresc." r g-.] |
	r [a-.\f bes-.\< \!c-.] |
	[d8.->\> \!bes16] [f8 d] |

	% four measures copied from 8 measures back...
	[d16-.\p  d-. d-. d-.] [d8-. c-.] |
	es2-> |
	[es16-. es-. es-. es-.] [es8-. d-.-"cresc."] |
	f2-> |
	[fis16-.\p  fis-. fis-. fis-.] [fis8-. g-.] |
	% same measure
	[fis16-.\p  fis-. fis-. fis-.] [fis8-. g-.] |
	[fis8-. g-. fis-. g-.] |
	[a-. bes-. a-. bes-. ] |
	[a-.\f bes-. a-. bes-. ] |
	[c-.-"cresc." f,-. g-. a-.] |
	[bes-. bes-. ces8.-> as16] |
	[f8-. ges-. es-. ces-.] |
	% `a deux
	[as-. bes-. ces8.-> ges16] |
	[es8-. ces'-. ges-. as-.] |
	bes\p r r4 |
	[bes16-.-"cresc." bes-. bes-. bes-.] [bes8-. ces->] |
	% same measure
	[bes16-. bes-. bes-. bes-.] [bes8-. ces->] |
	[bes'16-.\ff bes-. bes-. bes-.] [bes8-. ces16->] r16^"court"-\fermata |
	% ugh: eight measures rest (ugh: r1 -> four beats...)
	% eiht measures rest..
	\property Voice . textstyle =  "bold"
%	r2^"Un peu plus lent et \\'el\\'egant"
	R2*8
%	r4 r8\p bes |
	r4^"Un peu plus lent et \\'el\\'egant"
	r8\p bes,, |
%	g2->^"Un peu plus lent et \\'el\\'egant" ~ |
	g2-> ~ |
	\property Voice . textstyle =  "italic"
	g8 r r c |
	c2-> ~ |
	c8 r r bes'\f |
	[bes8.->\f g16-.] [es16-. bes-. g-. as-.] |
	[bes-.-"dim." c-. d-. f-.] [es8-. c-.] |
	d2-> ~ |
	[d16-"dim."( es f g] [)as,8 as-.] |
	[ces8.->\p( )bes16-.] bes4 ~ |
	[bes16( c! d f] [)es8 g,-.] |
	[g8.->( )as16] as4 ~ |
	[as16( a bes b] [)c8-. d,-.] |
	% a deux
	[d8.->( ) es16] es4 ~ |
	es4 r8 bes'-. |
	g2-> ~ |
	g8 r r c |
	c2 ~ |
	c8  r r bes' |
	[bes8.-> g16-.] [es16-. bes-. g-. as-.] |
	[bes-. c-. d-. es-.] [f-. g-. as-. bes-.] |
	d8-. r c4-> ~ |
	[c16 f,-. g-. a-.] [bes-. c,-. d-. es-.] |
	g8-. r f4-> ~ |
	[f16 bes,-. c-. d-.] [es-. f-. g-. as-.] |
	c8 r bes4 ~ |
	[bes8 as-. g8.-. e16-.] |
	g8-. r f4-> ~ |
	[f8 es-. ces8.-. as16-.] |
	% `a deux
	\property Voice . textstyle =  "large"
	[ces8.->^"Plus vite" bes16-.(] [a bes es d] |
	\property Voice . textstyle =  "italic"
	[)c!8.-> bes16] [a( bes es )d] |
	[c->( bes es )d] [c->( bes es )d] |
	% same measure
	[c->( bes es )d] [c->( bes es )d] |
	[c( bes a bes] [c d es e] |
	[g )f d( es] [f g as a] |
	[c )bes bes,( c] [d es f g] |
	[as g f g] [as bes c d] |
	\property Voice . textstyle =  "large"
	[)es-.^"Tempo 1$^o$" g,-. g-. g-.] [g8-. g-.] |
	\property Voice . textstyle =  "italic"
	bes4.-> g8-. |
	gis2->( |
	)as! |
	[g16-. g-. g-. g-.] [g8-. g-.] |
	bes4.-> g8-. |
	gis2->( |
%	)as! |
	)as |
	[bes16-. bes-. bes-. bes-.] [bes8-. bes-.] |
	bes4.-> g8-. |
	[g16-._"h\\^atez" g-. g-. g-.] [g8-. g-.] |
	g4.-> g8-. |
	es4.-> es8-. |
	bes4.-> bes8-. |
	\property Voice . textstyle =  "large"
	g'4.->^"Presto" g8-. |
	\property Voice . textstyle =  "italic"
	es4.-> es8-. |
	[bes-. bes-. es-. g-.] |
	[bes-. bes,-. es-. g-.] |
	bes-. r r4 |
	g8-. r r4 |
	g8-. r r4 |
}

hoboone = \notes {
	\hoboonestart
	\hobooneintro
	\hoboonemid
	\hoboonesecondstart
	\hobooneintro
	\hoboonelast
}

global = \notes{
	\key es;
	\time 2/4;
	\skip 4*110;
	\key g; |
	\bar "||";
	\skip 4*58;
	\key es; |
	\bar "||";
	\skip 4*220;
	\bar "|.";
}


$staff_hoboone = \type Staff = hoboonestaff <
	\global
	\property Staff.instrument = "oboe"
	% don't expand multi-bar rest
	\property Score.SkipBars = 1
	\hoboone
>

a4 = \paper{
	\translator{ \BarNumberingStaffContext }
}

a4sixteen = \paper{
	\paper_sixteen
	linewidth= 193.\mm;
	\translator { \BarNumberingStaffContext }
}

\score{
	\$staff_hoboone
	\paper{ \a4 }
	\midi{
		\tempo 4 = 80;
	}
%	\paper{ \a4sixteen }
}

