\version "1.0.20";

viI=\notes\relative c'' {

\property Staff."midiInstrument" = "violin"

r4 r [g8.-> \mf ( a16 ] |
) g4 r c, \p |
[c'8. ( d16 ] c4. a8 |
[f8. e16 ] f4. ) a8 |
[c8. ( d16 ] c4. ) a8 |
[a8. ( g16 ] ) f4 r |
[c'8. ( d16 ] c4. a8 |
[f8. e16 ] f4. ) a8 |
[c8. ( d16 ] c4. ) a8 |
[a8. ( g16 ] ) f4 r |
[c'8. \mf ( \< d16 ] [b8 c] \! ) f4 |
[ e8. \> ( d16 ] c4 \! ) g \pp |
[c8. ( \< d16 ] [b8 c] \! ) f4 |
[ e8. \> ( d16 ] ) \! c2 \p |
[c8. ( d16 ] c4. a8 |
[g8. a16 ] f4. ) a8 |
[c8. ( d16 ] c4. ) a8 |
[a8. ( g16 ] ) f4 r |
g2 \< ( \! f4 |
e \> \! ) f r |
r8 [ c ( g' c, f c ] |
[ bes c a8 c a ) c ] |
[a ( c a c a c ] |
[a \pp c a c a ) c ] |
[bes ( c ] bes4 [a8 c ] |
[ bes c ] ) bes2 ~ |
bes r4 |
R2.*4 |
r4 r^\fermata r8^\fermata [c'16 \pp ( d] | \time 2/4;
) c8 r r [c16 ( d] |
) c8 r r [c16 ( d] |
[c d c a][f a c d] |
[f8 d ) c d ( ] |
[ ) a bes () g d' ( ] |
[ ) a bes () g ] [d'16 ( es ] |
[ d es d bes][g bes d es] |
[ g8 es ) d bes ( ] |
[) g a () f c' ( ] |
[) g a () f ][ c'16 ( d ] |
[ ) c8 c16 ( d ][ ) c8 c16 ( d ] |
[ ) c8 c16 ( d ][ ) c8 c16 \f ( d ] |
[c d c a][f a c d] |
[f8 d ) c f, ( ] |
[f' d ) c a ( ] |
[a' f ) e ] [a,,16 ( b ] |
[c-> d c b ][ ) a8 a'16 ( b ] |
[c d c b ][ ) a8 a,16 ( b ] |
[c \> d c \! b ][ ) a8 a'16 ( b ] |
[c d c b ][ ) a8 a,16 ( b ] |
[) c8-. b16 ( c][ ) d8-. c16 ( d ] |
[ es f es ) d ][c8-. d'16 ( e ] |
[f! \fz g f e][d \> e d c ] |
[ b c b \! a][g a g ) fis ] |
f!4 ( \p \< g |
\! a2 |
a4 \> [g8 \! f'] |
[e c a ) g ] |
f4 ( \< g |
\! a2~ |
a4 \> ) g |
\! a2 ( |
[a8_"dim." g f d] |
<{[b g ][a ) g ] |} 
  \context Voice=x {s8.. \tiny b8}>
R2 |
r4 r8 a'-. \mf |
[a-. \< a-. a-. \! a-. ] |
b4. \fz r8 |
R2 |
r4 r8 a-. \mf |
[a-. \< a-. a-. \! a-. ] |
b4. \f bes8 ( |
) bes'4.-> bes,8 ( |
) bes'4.-> a8 \p | % added \p
[g-. \< f-. e-. \! d-. ] |
[cis16 ( e d b][ ) g8 \f bes8 (] |
) bes'4.-> bes,8 ( |
) bes'4.-> a8 \p |
[g-. \< f-. e-. \! d-. ] |
[g, g'16 \f ( a ][ ) g8 g,, \p ] |
g4^\trill ( a^\trill | % added longer slur and trills
b^\trill \tiny [a16*1/2 b*1/2] \normalsize[c8*1/2 ) g ] |
g4^\trill ( a^\trill |
b^\trill \tiny [a16*1/2 b*1/2] \normalsize [) c8*1/2 bes' \f ( ] | 
) bes'4.-> a8 |
[f-. e-. d-. f, ( ] |
< ) d'4. \fz a'> g,8 ( |
< ) d'4. \fz g> g,8 ( |
) e' r g4-> ( |
) e8 r g4-> ( |
) e8 r es4 ( |
[) d8 g16 ( a ] [ ) g8 g, ( ] |
) e'! r g4-> ( |
) e8 r g4-> ( |
) e8 r es4 ( |
[) d8 g16 ( a ] [ ) g8 g, ( ] |
) e' r g4_"dim." ( |
) e8 r d4 ( |
) e8 r g4 ( |
) e8 r d4 ( |
[ ) e8 \p g,-. a-. b-.] |
c r r4 |
R2 |
r4 e,-. \p |
a ( \< ) e' |
e-. () \! e-. |
e \fz \> [ d8 \! c ] |
b2 \p | % \p added
\tiny e8*1/2 \normalsize g4 ( \> *3/4 dis | 
\! e ) b |
\tiny e8*1/2 \normalsize g4 ( \> *3/4 dis | 
\! e ) b |
\tiny d8*1/2 \normalsize f2 \fz ( *7/8 |
b,4 [c8 ) b] |
a2-> ( |
) gis4 e |
a ( ) e' |
e-.  \< () \! e-. |
e ( \fz \> [ d8 \! c ] |
) b2 \p |
\tiny e8*1/2 \normalsize g4 ( \> *3/4 dis |
\! e ) b |
\tiny e8*1/2 \normalsize g4 ( \> *3/4 dis |
\! e ) b |
\tiny d8*1/2 \normalsize f2 \fz ( *7/8 |
b,4 [c8 ) b] |
a2 \p ( |
) gis |
\tiny a8*1/2 \normalsize d2 ( \> *7/8 |
b4 [ c8 \! b ] |
a2 \p |
) gis |
R2*4 |
[c16 ( \pp d c a][f a c d] |
[f8 d ) c d ( ] |
[ ) a bes () g d' ( ] |
[ a ) bes g ] [d'16 ( es ] |
[ d es d bes][g bes d es] |
[ g8 es ) d bes ( ] |
[) g a () f c' ( ] |
[) g a () f ][ c'16 ( d ] |
[ ) c8 c16 ( d ][ ) c8 c16 ( d ] |
[ ) c8 c16 ( d ][ ) c8 c16 \ff ( d ] |
[c d c a][f a c d] |
[f8 d ) c f, ( ] |
[f' d ) c ][f,16 ( g] |
[ ) f8 f16 ( g][ ) f8 f'16 ( g] |
[f g f d][bes d f g] |
[bes8 g ) f bes, ( ] |
[bes'8 g ) f d ( ] |
[d' bes ) a d,,16 ( e ] |
[f g f e ][ ) d8 d'16 ( e ] |
[f-> g f e][ ) d8 d,16 ( e ] |
[f g f e ][ ) d8 d'16 ( e ] |
[f g f e][ ) d8 d,16 ( e ] |
[ ) f8 e16 ( f][ ) g8 f16 ( g ] |
[as bes as g][ ) f8 g'16 ( a! ] |
[bes! \fz c bes a][g a g ) f ] |
[e ( f e d][c d c ) b ] |
bes!4 \p ( \< c | % \p added
\! d2 ~ |
d4 [c8 bes' \> ] |
[a f d ) \! c ] |
bes4 ( \< c |
\! d2 ~ |
d4 \> c |
\! d2 |
[d8 c][bes g] |
[e c] \tiny e8*1/4 \normalsize [d *3/4 ) c ] |
R2 |
r4 r8 d' \mf |
d2:8 \< |
\! e4 \f r |
R2 |
r4 r8 d |
d2:8 \< |
\! e4 \f r8 es ( |
) es'4.-> es,8 ( |
) es'4.-> d8 \p |
[c-. \< bes-. a-. \! g-. ] |
[fis16 ( a g ) e!] [c8 es8 ( \f ] |
) es'4.-> es,8 ( |
) es'4.-> d8 \p |
[c-. \< bes-. a-. \! g-. ] |
[c, c'16 ( \f d] [) c8 c,, \p ] |
c4^\trill ( d^\trill |
e^\trill \tiny [d16*1/2 e*1/2] \normalsize [f8 *1/2 ) c ] |
c4^\trill ( d^\trill |
e^\trill \tiny [d16*1/2 e*1/2] \normalsize [f8 *1/2 ) es' \f ( ] |
) es'4. d8 |
[bes-. a-. g-. bes, ( ] |
) d'4. \fz c,8 ( |
) c'4. \fz c,8 ( |
) a' r c4-> ( |
) a8 r c4-> ( |
) a8 r as4 ( |
[) g8 c16 ( d][) c8 c, ( ] |
) a' r c4 ( |
) a8 r c4 ( |
) a8 r as4 ( |
[) g8 c16 ( d][) c8 c, ( ] |
) a' r c4_"dim." ( |
) a8 r g4 ( |
) f8 r c4 ( |
) a8 r g4 \p ( |
) f2 \< ~ |
\! f2 ( ~ |
f4 g |
f ) e |
es2 ( |
) d4 r |
\tiny f'8*1/2 \normalsize a4 \fz ( *3/4 e |
f b, |
) c r |
R2 |
f,8 \p r r4 | % \p added
e8 r r [c'16 ( \pp d] |
[ ) c8-. b16 ( c ][ ) b8-. bes16 ( c ] |
[ ) bes8-. a16 ( bes ][ ) a8-. c16 ( d ] |
[ ) c8-. b16 ( c ][ ) b8-. bes16 ( c ] |
[ ) bes8-. a16 ( bes ][ ) a8-. e'16 ( f ] |
[ ) e8-. d16 ( e ][ ) d8-. c16 \< ( d ] |
[ ) c8-. b16 ( \! c ][ ) b8-. bes ] |
bes'4. \fz \> a8 |
[g-. \! f-. ] [e-. c16 ( d ] | 
[ ) c8-. b16 ( c ][ ) b8-. bes16 ( c ] |
[ ) bes8-. a16 ( bes ][ ) a8-. c16 ( d ] |
[ ) c8-. b16 ( c ][ ) b8-. bes16 ( c ] |
[ ) bes8-. a16 \< ( bes ][ ) a8-. \! bes ( ] |
) bes'4. \f d,8 ( |
) d'4.-> c,8 ( |
) c'4.-> e,8 ( |
) e'4.-> [c16 ( d] |
[c d c a][f a c d] |
[f8 d ) c ][ c16 ( d ] |
[c d c a][f a c d] |
[f8 d ) c ][f,16 ( g ] |
[f g f d ][ ) b8 bes'!16 ( c ] |
[bes c bes g][ ) e8 c'16 ( d ] |
[) c8 c16 ( d ][) c8-. c16 ( d ] |
[) c8-. c16 ( d ][) c8-. c16 ( d ] |
) c8-. r c r |
a r <e bes> r |
<a, f'> r a' r |
c r a r |
<f4 a,> r8 c ( |
<a4 ) a'> r8 c ( |
<a4 ) f'> r8 c ( |
<a4 ) a'> r8 c, |
f2 ~ |
f ( |
<f4 \fz c' ) a'> r^\fermata

}

viII=\notes\relative c' {

\property Staff."midiInstrument" = "violin"

r4 r e->  \mf ~ |
e r e ( \p |
) f2 f4 ( |
) c r r |
f2 f4 ( |
) e f r |
f2 f4 ( |
) c r r |
f2 f4 ( |
e \< [f8. d16][c8. \! ) d16] |
[e8. \mf ( f16][d8 ) e] f4 ~ |
f ( e [c8 ) e] |
a2. \pp ( |
gis4 e ) f |
g! \p [f8. ( g16] ) f8 r |
c4 ( ~ [c8. d16] ) c8 r |
g'4 [f8. ( g16] ) f8 r |
c4 r r |
<c2. { s4. \< \! s4. \>}> ~ |
\! c2._"dim." ~ |
c \pp ~ |
c \< ~ |
\! c \> ~ |
\! c \pp \< ~ |
\! c \> ~ |
\! c \pp ~ |
c2 r4 |
R2.*4 |
r4 r^\fermata r8^\fermata [c'16 \pp ( d] | \time 2/4;
) c8 r r [c16 ( d] |
) c8 r r [c16 ( d] |
[c d c a][f a c d] |
[f8 d ) c ] r |
fis,-. r g-. r |
fis r [g d'16 ( es ] |
[ d es d bes][g bes d es] |
[ g8 es ) d ] r |
e,! r f r |
e r f r |
R2 |
r4 r8 [ c'16 \f ( d ] |
[c d c a][f a c d] |
[f8 d ) c f, ( ] |
[f' d ) c f, ( ] |
[f' d ) c ] [a,16 ( b ] |
[c d c b ][ ) a8 a'16 ( b ] |
[c-> d c b ][ ) a8 a,16 ( b ] |
[c d c b ][ ) a8 a'16 ( b ] |
[c-> d c b ][ ) a8 a,16 ( b ] |
[) c8-. b16 ( c][ ) d8-. c16 ( d ] |
[ es f es ) d ][c8-. d'16 ( e! ] |
[f! g f e][d e d c ] |
) b r r8 r4 |
<{d,2 ( \p ~ |
  d4 e |
  f2 |
  ) e |
  d ( \p ~ |
  d4 e |
  f2 |
  f |
  ) f4 }
 {b,2 ( ~ |
  b4 c |
  b2 |
  ) c |
  b ( ~ |
  b4 c |
  b2 |
  b4 c |
  ) d \p } > r |
R2*2 |
r4 r8  fis-. \mf |
[fis-. \< fis-. fis-. \! fis-. ] |
f!4. \f r8 |
R2 |
r4 r8  fis-. |
fis2:8 |
f!4. \f e8 ( |
) e'4.-> e,8 ( |
) e'4.-> c,8 \p |
[cis-. \< d-. e-. \! f-. ] |
f4. \f e8 ( |
) e'4.-> e,8 ( |
) e'4.-> c,8 \p |
[cis-. \< d-. e-. \! f-. ] |
[f \f g16 ( a][ ) g8 g, \p ] |
g4^\trill ( \< \! a^\trill |
b^\trill \> \tiny [a16*1/2 b*1/2] \normalsize [ ) c8 *1/2 \! g] |
g4^\trill ( \< \! a^\trill |
b^\trill \> \tiny [a16*1/2 b*1/2] \normalsize [ ) \! c8 *1/2 g' \f ( ] |
< ) g4.-> bes> a8 |
[a a a f ( ] |
<d'4. \fz ) a'> g,8 ( |
<d'4. \fz ) g> g,8 ( |
<{ ) e r f4_> ( |
  ) e8 r f4_> ( |
  ) e8 }
 {c'8 s d4 ( |
  ) c8 s d4 ( |
  ) c8 }> r fis4 ( |
[ ) f!8 g16 ( a][ ) g8 f ( ] |
<{ ) e! r f4_> ( |
  ) e8 r f4_> ( |
  ) e8 }
 {c'8 s d4 ( |
  ) c8 s d4 ( |
  ) c8 }> r fis4 ( |
[ ) f!8 g16 ( a][ ) g8 f ( ] |
<{ ) e r f4 ( |
  ) e8 r f4_"dim." ( |
  ) e8 r f4 ( |
  ) e8 r f4 ( |
  [ ) e8 \p }
 {c'8 s d4 ( |
  ) c8 s b4 ( |
  ) c8 s d4 ( |
  ) c8 s b4 ( |
  [ ) c8 }> g,-. a-. b-. ] |
c r r4 |
R2*2 |
a4:16 \p \< \! b4:16 \> |
\! c2:16 \< |
\! b2:16 \> |
\! b2:16 |
b4:16 \p \< \! c4:16 \> |
\! b2:16 |
b4:16 \p \< \! c4:16 \> |
\! b2:16 |
\tiny d8*1/2 \normalsize f2 ( \> *7/8 | % grace note c8 corrected to d8
\! ) e |
b-> ( |
) e4 r |
a,4:16 \p b4:16 |
c2:16 \< |
\! b2:16 \fz \> |
\! b2:16 |
b4:16 \p \< \! c4:16 \> |
\! b2:16 |
b4:16 \p \< \! c4:16 \> |
\! b2:16 |
\tiny d8*1/2 \normalsize f2 ( \fz *7/8 |
) e |
[ a,16 ( \p d a d ][a d a d] |
[ b16 d b d ][b d b ) d] |
f2 ( \> |
) \! e | 
[ a,16 ( d a d ][a d a d] |
[ b16 d b d ][b d b ) d] |
R2 |
r8 [e16^"solo" ( f] ) e4 ( |
) d2_"dim." ~ |
d ( |
) c4 r |
r r8 d' ( |
[ ) a bes () g d' ( ] |
[ a ) bes g ] r |
R2 |
r4 r8 bes ( |
[) g a () f c' ( ] |
[) g a () f ] r |
R2 |
r4 r8 [c'16 \fz ( d ] |
[c d c a][f a c d] |
[f8 d ) c f, ( ] |
[f' d ) c ][f,16 ( g] |
[ ) f8 f16 ( g][ ) f8 f'16 ( g] |
[f g f d][bes d f g] |
[bes8 g ) f bes, ( ] |
[bes' g ) f bes, ( ] |
[bes' g ) f d,16 ( e! ] |
[f-> g f e ][ ) d8 d'16 ( e ] |
[f g f e][ ) d8 d,16 ( e ] |
[f g f e ][ ) d8 d'16 ( e ] |
[f g f e][ ) d8 d,16 ( e ] |
[ ) f8 e16 ( f][ ) g8 f16 ( g ] |
[as bes as g][ ) f8 g'16 ( a! ] | % misprint fz to early
[bes! \fz c bes a][g a g ) f ] |
e r r8 r4 |
<{e,2 \p ~ |
  e4 \< \! f |
  e2 \> |
  \! f |
  e \< |
  e4 \! f |
  e2 ( |
  ) e4 ( f |
  ) g4 \p }
 {g2 ( ~ |
  g4 a |
  bes2 |
  ) a |
  g2 |
  g4 a |
  bes2 ( 
  ) bes2 (
  ) bes4}> r |
R2*2 |
r4 r8 b \p |
b2:8 \< |
< \! g4 bes! \fz > r |
R2 |
r4 r8 b \p |
b2:8 \< |
< \! g4 bes! \f > r8 a ( |
<c4._> ) a'> a8 ( |
<c4._> ) a'> f,8 \p |
[fis-. g-. a-. bes-.] |
bes4. a8 ( \f |
<c4._> ) a'> a8 ( |
<c4._> ) a'> f,8 \p |
[fis-. \< g-. a-. \! bes-.] |
[bes \f c16 ( d][)c8 c, \p ] |
c4^\trill ( d^\trill |
e^\trill \tiny [d16*1/2 e*1/2] \normalsize [ f8 *1/2 ) c ] |
c4^\trill ( d^\trill |
e^\trill \tiny [d16*1/2 e*1/2] \normalsize [ ) f8 *1/2 a ( \f ] |
<a4. ) c4.> d8 |
[d d d bes \f (] |
<bes4. ) g'4.> bes8 (|
<bes4. \f ) g'4.> bes8 (|
<{a r bes4 ( |
  ) a8 r bes4 ( |
  ) a8}
 { ) f' r g4^> ( |
  ) f8 r g4^> ( |
  ) f8}> r b4 ( |
[ ) bes!8 c16 ( d][ ) c8 bes ( ] |
<{a r bes4 ( |
  ) a8 r bes4 ( |
  ) a8}
 {) f' r g4 ( |
  ) f8 r g4 ( |
  ) f8}> r b4 ( |
[ ) bes!8 c16 ( d][ ) c8 bes ( ] |
<{a r bes4_"dim." ( |
  ) a8 r bes4 ( |
  ) a8}
 {) f' r g4 ( |
  ) f8 r e4 ( |
  ) f8}> r g4 ( |
) c,8 r e4 \p |
es2 ( |
) d4 \< \! des |
c2 \> ~ |
\! c |
a4 ( \< c |
d \! des |
c \f \> \! ) bes |
a ( \> g |
\! ) a r |
R2 |
c8 \p r r4 |
c8 r r4 |
R2 |
r4 r8 c \p ~ |
c d4 e8 ~ |
e f4 c8 ~ |
c d4 e8 ~ |
e \< e4 \! e8 ( |
) bes'!4. \fz \> \! a8 |
[g-. f-. e-.] r |
R2 |
r4 r8 es \p ~ |
es es4 es8 ~ |
es \< es4 \! d8 \f |
  d'4.-> <{bes8 ( |
  ) g'4. bes,8 ( |
  ) g'4. bes,8 ( |
  ) g'4. }
 {d,8 |
  bes'4. g8 |
  bes4._> g8 |
  bes4._> }> [c,16 ( d] |
[c d c a][f a c d] |
[f8 d ) c ][ c16 ( d ] |
[c d c a][f a c ) d] |
[f8 ( d ) c ][f16 ( g ] |
[f g f d ][ ) b8 bes!16 ( c ] |
[bes c bes g][ ) e8 < bes' g'^. > ] |
<{[a bes a bes] |
  [a bes a bes] |
  [a}
 {[f'-. g-. f-. g-.] |
  [f-. g-. f-. g-.] |
  [ f}> c16 ( d ][ ) c8-. c16 ( d ] |
[ ) c8-. c16 ( d ][ ) c8-. c16 ( d ] |
) c8-. r <c f,> r |
 <c f,> r <c f,> r |
<a4 f> r8 c ( |
<a4 ) a'> r8 c ( |
<a4 ) f'> r8 c ( |
<a4 ) a'> r8 c, |
[a16 ( gis a gis][a gis a gis] |
[a gis a gis][a gis a gis] |
<) a4 f' \fz > r^\fermata 
}

vla=\notes\relative c' {
\clef "alto";

\property Staff."midiInstrument" = "viola"

[c8. \f ( d16 ] ) c2_"dim." |
\context Staff <{\voiceone s4 c2 |
  c2. ~ |
  c2. ~ |
  c2. ~ |
  c2. ~ |
  c2. ~ |
  c2. ~ |
  c2. ~ | } 
 { \voicetwo [c8. ( d16 ] c4 ) bes |
  [a8 \p ( bes ] a4. ) c8 | % added \p 
  [a8 ( bes ] a4. ) c8 |
  [a8 ( bes ] a4. ) c8 |
  [c8 ( bes ] ) a2 |
  [a8 ( bes ] a4. ) c8 |
  [a8 ( bes ] a4. ) c8 |
  [a8 ( bes ] a4. ) c8 |} >
[c8. ( \< bes16 ] ) \! a2 |
g2 \mf [a8 \< ( \! d ] |
[ c \> b \! c g e ) c] |
[e'8. \pp ( f16 d8 e f d ] |
b4 [c8 \< a f \! ) a ] |
c,2 \p ~ c'4 |
c,2 ~ c'4 |
c,2 ~ c'4 |
c, ~ c' r |
[c,8 ( g' ][ c g c a ] |
[bes c ] ) a4 r |
R2. |
r4 r dis, \< ( |
) \! e r dis' \pp ( |
) e r r |
R2. |
r4 r c, \pp ~ |
c2. ~ |
c4 r r |
R2.*3 |
r4 r^\fermata r8^\fermata r8 |
R2*4 |
c'8-. \pp r bes-. r |
c-. r bes-. r |
R2*2 |
bes8-. r a-. r |
bes-. r [a-. c16 ( d ] |
[ ) c8 c16 ( d ] [ ) c8 c16 ( d ] |
[ ) c8 c16 ( d ] [ ) c8 c16 () f ] |
[c ( \f f c f][ c f c ) f ] |
[d ( f d f][ c f c ) f ] |
[d ( f d f][ c f c ) f ] |
[d ( f d f] ) a8 r |
f,4.-> [a16 ( b] |
[c d c b] ) a8 r |
f4. [a16 ( b] |
[c d c b] ) a8 r |
<f8 a> r <f a> r |
<fis a> r <fis a> r |
[<g \fz d'> d'16 ( e][f g f e ] |
[d e d c] ) b8 r |
[as16 ( \p g as g] [fis g fis ) g ] |
[f! ( g f g][e g e ) g] |
d4 \> \! g |
[c,16 ( g' a g ] [a g a ) g ] |
[as16 ( \p g as g] [fis g fis ) g ] |
[f! ( g f g][e g e ) g] |
[d ( g d g ][d g d g ] |
[d g d g ] [c, g' c, g' ]
) d4 r |
r r8 <c \p g'> |
<c2:8 g'2:8 > |
<c2:8 g'2:8 \< > |
< \! c2:8 \f g'2:8 > |
<{[c8 c c c \p ]}{[g' g g g]}> |
<c2:8 g'2:8 > |
<c2:8 g'2:8 > |
<c2:8 \f g'2:8 > |
<{[c8 c c c ] |
  c'4.-> c,8 |
  c'4. }
 {[g8 g g g (] |
  ) g'4. g,8 ( |
  ) g'4. }
     > a'8-. \p |
[g-. \< f-. e-. \! d-. ] |
b4. <{c,8 \f |
  c'4.-> c,8 |
  c'4. }
 { g8 ( |
  ) g'4. g,8 ( |
  ) g'4. }> a'8-. \p |
[g-. \< f-. e-. \! d-. ] |
b4 r8 <c, \p g'> |
<c2:8 g'2:8 > |
<c2:8 g'2:8 \< > |
< c2:8 g'2:8 > |
<{[ \! c8 c c cis \f (]}{[g' g g s ]}> |
) cis'4.-> cis8 |
[d a d <d ( g,> ] |
<f4. ) d' \fz > <d8 ( g,> |
<f4. ) d' \fz > g,8 ( |
[c,16 g' fis ) g] [c, (g' fis ) g ] |
[c, ( g' fis ) g] [c, (g' fis ) g ] |
[c, ( g' fis ) g] [c, (a' gis ) a ] |
[b ( c d ) b ][g ( a f ) d ] |
[c ( g' fis ) g] [c, (g' fis ) g ] |
[c, ( g' fis ) g] [c, (g' fis ) g ] |
[c, ( g' fis ) g] [c, (a' gis ) a ] |
[b ( c d ) b ][g ( a f ) d ] |
[c ( g' fis ) g] [c, (g' fis ) g ] |
[c,_"dim." ( g' fis ) g] [c, (g' fis ) g ] |
[c, ( g' fis ) g] [c, (g' fis ) g ] |
[c, ( g' fis ) g] [c, (g' fis ) g ] |
c,8 \p r r4 |
r8 [g'-. a-. b-.] |
c r r4 |
r8 <{[b c d] | c }{gis-. a-. b!-. | a}>
  r r4 |
R2*2 |
g'4 \p ( [fis8 \< dis ] |
\! e4 fis |
g \> [fis8 ) \! dis ] |
e4 \< ( \! fis |
g \> [fis8 ) \! dis ] |
<d!2 \fz e, > ~ |
<d2 e, > |
<d b > ~ |
<d b > |
c4 r |
R2*2 |
g'4 \p ( [fis8 \< dis ] |
\! e4 fis |
g \> [fis8 ) \! dis ] |
e4 \< ( \! fis |
g \> [fis8 ) \! dis ] |
<d!2 \f e, > ~ |
<d e, > |
d \p ~ |
d |
<d a \> > ~ |
<d \! gis, > |
d \p ~ |
d ~ |
d ~ |
d_"dim." ~ |
d4 r |
r8 [e16 ( f ] e4 |
) es \pp r |
R2*3 |
g,2 ~ |
g4. r8 |
R2 |
r4 r8 [c16 \p ( d ] |
[ ) c8 c16 ( d ] [ ) c8 c16 ( d ] |
[ ) c8 c16 ( d ] [ ) c8 c16 ( f ] |
[c \fz f c f][ c f c ) f ] |
[d ( f d f][ c f c ) f ] |
[d ( f d f] ) c8 r |
r4 r8 [ f16 () g ] |
<d2:16 f2:16> |
<es4:16 g4:16> <d4:16 f4:16> |
<es4:16 g4:16> <d4:16 f4:16> |
<g4:16 bes4:16> <f!8 a> r |
bes,4.-> [d16 ( e ] |
[f g f e ] ) d8 r |
bes4.-> [d16 ( e ] |
[f g f e ] ) d8 r |
<bes d> r <bes d> r |
<b d> r <b d> r |
[<c \fz e^. > g'16 ( a ] [bes! \> c \! bes a ] |
[g a g f ] ) e r r8 |
[des16 ( \p c des c ] [ b c b c ] |
[bes! \< c bes c ] [ \! a c a ) c ] |
g4 \> \! c, ( |
[f16 c' d c][d c d ) c] |
[des16 ( \p c des c ] [ b c b c ] |
[bes! \< c bes c ] [ \! a c a ) c ] |
[g \> ( c g c][g c g \! c ] |
[g c g c][f, c' f, ) c'] |
e,4 \p r |
r r8 <f \p c'> |
<f2:8 c'2:8> |
<f2:8 \< c'2:8> |
<f2:8 \! c'2:8> |
<{[f8 \f f f f \p ]}{[c'8 c c c]}> |
<f2:8 \< c'2:8> |
<f2:8 \! c'2:8> |
<f2:8 \f c'2:8> |
<{[f8 f f f] |
  f'4._> f,8 |
  f'4._> }
 {[c8 c c c ( ] |
  ) c'4. c,8 ( |
  ) c'4. }> d'8 \p |
[c-. \< bes-. a-. \! g-. ] |
<{c,4. f,8 \f |
  f'4._> f,8 |
  f'4. }
 {e4. c8 ( |
  ) c'4. c,8 ( |
  ) c'4. }> d'8 \p | % \p added
[c-. \< bes-. a-. \! g-. ] |
<e4 c> r8 <f, \p c'> |
<f2:8 c'2:8> |
<f2:8 \< c'2:8> |
<f2:8 \! c'2:8> |
<{[f8 f f fis \f (( ]}{[c'8 c c s]}> |
< ) a4. ) fis'4.> fis'8 |
[g d g <g, ( c,> ] |
<c4. \fz ) g'> <g8 ( c,> |
<c4. \fz ) g'> c,8 ( |
[f16 c' b ) c ][ f, ( c' b ) c ] |
[ f, ( c' b ) c ] [ f, ( c' b ) c ] |
[ f, ( c' b ) c ] [ f, ( d' cis ) d ] |
[e ( f g ) e ][c ( d bes ) g] |
[ f ( c' b ) c ] [ f, ( c' b ) c ] |
[ f, ( c' b ) c ] [ f, ( c' b ) c ] |
[ f, ( c' b ) c ] [ f, ( d' cis ) d ] |
[e ( f g ) e ][c ( d bes ) g] |
[ f ( c' b ) c ] [ f, ( c' b ) c ] |
[ f,_"dim." ( c' b ) c ] [ f, ( c' b ) c ] |
[ f, ( c' b ) c ] [ f, ( c' b ) c ] |
[ f, ( c' b ) c ] [ f, ( c' b ) c ] |
f,2 \p ~ |
f4 r |
R2*2 |
f'2 ( ~ |
f |
f4 \f g |
) f [f8. ( \> g16 ] |
\! f2 |
) c |
a8 \p r r4 |
g8 r r a \pp ~ |
a as4 g8 ~ |
g f4 a!8 ~ |
a as4 g8 ~ |
g f4 e8 ~ |
e \< f4 e8 ~ |
\! e d4 c8 ( |
<g'4. \fz ) e'> f8 |
[g-. a-. bes-. a ~ ] |
a as4 g8 ~ |
g f4 a!8 ~ |
a \< as4 \! g8 ~ |
g f4 \f f8 ( |
<bes4._> ) f' > g8 ( |
<d'4._> ) bes' > e,!8 ( |
<c'4._> ) g' > <g8 ( c, > |
<c4._> ) g' > c,8 ( |
) f4. a8 ( |
[d f ) a ] c,,8 ( |
) f4. a8 ( |
[d f ) a ] [f16 ( g ] |
[f g f d ][ ) b8 bes'!16 ( c ] |
[bes c bes g][ ) e8 c'16 ( d ] |
[) c8-. c16 ( d ][) c8-. c16 ( d ] |
[) c8-. c16 ( d ][) c8-. c16 ( d ] |
[) c8-. c,16 ( d ][) c8-. c16 ( d ] |
[) c8-. c16 ( d ][) c8-. c16 ( d ] |
) c8-. r c-. r |
a r c-. r |
<c4 a'> r8 c ( |
<f4 ) c'> r8 c, ( |
<c'4 ) a'> r8 c ( |
<f4 ) c'> r8 c, |
[c'16 ( b c b][c b c b] |
[c b c b][c b c b] |
<c4 \fz ) a'> r^\fermata 
}

vlc=\notes\relative c' {
\clef "bass";

\property Staff."midiInstrument" = "cello"

r4 r [bes8. \mf ( c16] |
) bes4 r bes \p ( |
[ a8 bes ) a f, ( c' f] |
[ a8 bes ) a f, ( c' f] |
[ a8 bes ) a f, ( c' f] |
[c' bes ) a f, ( c' f] |
[ a8 bes ) a f, ( c' f] |
[ a8 bes ) a f, ( c' f] |
[ a8 bes ) a f, \< ( c' f] |
[c' bes a f \! c ) f, ] |
<{g2. ( | ) g2 }
 {c,2. \mf ( | ) c2 \> ( ) \! c'4 |}
 {s2. \< \! s8}>
a2  \pp d4 \< ( |
e ) \! a2 |
[bes8 \p ( c] [a c a c] |
[bes8 c] [a c a c] |
[bes8 c] [a c a ) c] |
[c ( bes a f c ) f, ] |
e2 (\< \! f4 |
g \> ) \! f r |
e'2 ( f4 |
g ) f f, ( \< | % added slur
) \! e2 f'4 ( |
) e2 dis4 ( \< |
\! ) e2 dis4 ( \pp |
) e2. ~ |
e2 r4 |
R2.*4 |
r4 r4^\fermata r8^\fermata [c'16 \pp ( d] | \time 2/4; % \pp added
) c8 r r [c16 ( d] | 
) c8 r r [c16 ( d] |
[c d c a][f a c d] |
[f8 d ) c] r |
d,-. \pp r g-. r |
d-. r [g-. d'16 ( es] |
[d es d bes][g bes d es] |
[g8 es ) d] r |
c,-. r f-. r |
c-. r [f-. c'-. ] |
[bes-. a-. g-. f-.] |
[e d][c a'16 \f ( f] |
[a f a f][a f a ) f] | % Misprint trem 8
[bes ( f bes f][a f a ) f] |
[bes ( f bes f][a f a ) f] |
[a ( d, a' d,][)a8 a'16 ( b] |
[c d c b ][) a8 f ( ] |
) e4. [a16 ( b] |
[c d c b ][) a8 f ( ] |
) e4. [a,16 ( b ] |
[)c8 b16 ( c][)d8 c16 ( d] |
[es f es d] ) c8 r |
[g \fz d''16 ( e] [f g f e] |
[d e d c][b c b ) a] |
[as \p ( g as g][fis g fis g ] |
[f! \< g f g][e g e ) \! g] |
[d \> ( g d g] [d g d ) \! g] |
c,2 |
[as'16 ( g as g][fis g fis g ] |
[f! g f g][e g e ) g] |
[d ( g d g] [d g d g] |
d4 c \p |
) b r |
R2*2 |
r4 r8 c'8 \f ( |  % Misprint, missing slur
) c,4. c8 ( |
) c,4. r8 |
R2 |
r4 r8 c''8 \f (|
) c,4. c8 ( |
) c,4. c''8 ( |
< ) g4. c,_> > c8 ( |
< ) g4. c,> c,8 \p |
[cis-. \< d-. e-. \! f-. ] |
g,4.  c8 \f ( |
< ) c4. f,_> > c8 ( | % slur added
< ) c4. f,> c8-> | 
[cis-. \p \< d-. e-. \! f-. ] |
g,4 r8 <{ c8 \p |
  c2:8 |
  c2:8 \< |
  \! c2:8 |
  [c8 c s s] |
  a4.}
 {g'8 |
  g2:8 |
  g2:8 |
  g2:8 |
  [g8 g c cis] |
  e,4.-> }> a8 |
[d-. e-. f-. b ( ] |
) b,4. \f b'8 ( |
< ) d,4.  \f g,> g8 ( |
[)c,16 g' (fis) g][c,( g' fis ) g] |
[c, ( g' fis ) g][c, ( g' fis ) g] |
[c, ( g' fis ) g][c, ( a' gis ) a] |
[b ( c d ) b][g ( a f )d ] |
[c ( g' fis ) g][c, ( g' fis ) g] |
[c, ( g' fis ) g][c, ( g' fis ) g] |
[e ( g fis ) g][c, ( a' gis ) a] |
[b ( c d ) b][g ( a f )d ] |
[c ( g' fis ) g][c, ( g' fis ) g] |
[c, ( g' fis ) g][c, ( g' fis ) g] |
c,4_"dim." r |
c4 r |
c8 r8 r4 |
R2 |
r8 [c-.^"solo" \p d-. dis-. ] |
e r e, r |
c'4:16 \p d4:16 |
e4:16 \< \! fis4:16 |
g!2:16 \> |
g4:16 \! a4:16 |
g4:16 \< \! a4:16 |
g4:16 \> \! a4:16 |
g4:16 \< \! a4:16 |
g4:16 \> \! a4:16 |
gis2 \fz ~ |
gis |
f! ( |
[e8 d c ) b ] |
c4:16 \p d4:16 |
e4:16 \< \! fis4:16 |
g!2:16 \> |
\! g4:16 \p \< \! a4:16 |
g4:16 \> \! a4:16 |
g4:16 \< \! a4:16 |
g4:16 \> a4:16 |
g4:16 \! a4:16 |
gis2 \fz ~ |
gis ( |
) f \p \> |
\! e_"dim." |
d |
e |
f |
e |
[f16 ( g f g][f g f8 ] |
) e4 r |
[f16 ( g f_"dim." g][f g f8 ] |
) e4 r |
[c'16 ( d c a][f a c ) d] |
[f8 ( d ) c] r |
fis,8-. r g-. r |
d r [g d'16 ( es] |
[d es d bes][g bes d es] |
[g8 es ) d] r |
c, r f-. r |
c r [f-. c'-. ] |
[bes-. a-. g-. f-.] |
[e d][c a'16 \f ( f] |
[a f a f][a f a ) f] | % Misprint trem 8
[bes ( f bes f][a f a ) f] |
[bes ( f bes f][) a8 f-.] |
[e-. es-. d-. c-. ] |
[bes16 ( f' bes f][bes f bes ) f] |
[bes, ( a bes a][bes d f ) bes ] |
[bes, ( a bes a][bes d f ) bes ] |
g4 ( [ ) d8 d'16 ( e] |
[f g f ) e] [d8 bes^._> ] |
a4. [d16 ( e] |
[f g f ) e] [d8 bes ( ] |
) a4. [d,16 ( e ] |
[) f8-. e16 ( f] [) g8-. f16 ( g] |
[)as ( bes as g ] ) f8 r |
c \fz r r [e'16 ( f ] |
[g a_"dim." g f][e f e ) d] |
[des \p \< ( c des c][b c b \! ) c ] |
[bes! ( c bes c][a c a ) c ] |
[g ( c g c][g c g ) c] | % misprint a c a c a c a c, comp. bar 59
f,2 |
[des'16 \p ( c des c][b c b ) c ] |
[bes! ( c bes c][a c a ) c ] |
[g ( c g c][g c g c] | % misprint a c a c a c a c, comp. bar 64
g4 f |
) e r |
R2*2 |
r4 r8 f' \f (|
) f,4.-> f8 ( |
) f,4.-> r8 |
R2 |
r4 r8 f'' (|
) f,4.-> f8 ( |
) f,4.-> f'8 (|
<) c4. f, > f8 ( |
<) c4. f,> f8 \p |
[fis-. \< g-. a-. \! bes-. ] |
c,4.  f8 \f ( |
<) f4. bes,_> > f8 (|
<) f4. bes,> f8 \p | % \p added
[fis-. \< g-. a-. \! bes-. ] |
c,4 r8 <{ f8 \p |
 f2:8 \< |
 f2:8 |
 \! f2:8 | }
 {c'8 |
 c2:8 |
 c2:8 |
 c2:8 |}>
 [c'8 c f fis ( \f ] |
 < ) a,4. d,_> > d,8 |
[g-. a-. bes-. e! ( ] |
) e,4.-> e'8 |
< g,4.  c,_> > c8 |
[f,16 ( c' b c][f,16 c' b ) c] |
[f,16 ( c' b c][f,16 c' b ) c] |
[f,16 ( c' b ) c][f, ( d' cis ) d] |
[e ( f g ) e][c ( d bes ) g] |
[f16 ( c' b ) c][f,16 ( c' b ) c] |
[f,16 ( c' b ) c][f,16 ( c' b ) c] |
[f,16 ( c' b ) c][f, ( d' cis ) d] |
[e ( f g ) e][c ( d bes ) g] |
[f16 ( c' b ) c][f,16 ( c' b ) c] |
[f,16 ( c' b ) c][f,16 ( c' b ) c] |
f,8 r e'4 ( |
) f8 r bes,4 ( |
) a2 ( |
) bes |
a4 \< ( \! bes |
a \> ) \! g |
a2 ( \< |
) \! bes |
f4 \f \> ( cis |
d \! g, |
) c!8 r r4 |
R2 |
c8 r r4 |
c8 r r4 |
<c8 \pp f,> r <c f,> r |
<c f,> r <c f,> r |
<c f,> r <c f,> r |
<c f,> r <c f,> r |
e, r e r |
e r [e () c] |
c'4. \f \> \! d8 |
[e-._"dim." f-. g-.] r |
<c,8 \p f,> r <c f,> r |
<c f,> r <c f,> r |
<c f,> r <c f,> r |
<c f,> r <c f,> bes' \f |
bes,4.-> g'8 ( |
) g,4.-> e'!8 ( |
) e,!4.-> c'8 ( |
) c,4.-> [c''16 ( d ] |
[c d c a][f a c d] |
[f8 d ][) c c16 ( d] 
[c d c a][f a c d] |
[f8 d ][) c d, ( ] |
) g,4.-> g'8 ( |
) c,4.-> c'8 ( |
[ ) f, c-. f-. c-. ] |
[f-. c-. f-. c-. ] |
f r c r |
f r c' r |
f, r c r |
a r c r |
f4 r8 c8 ( |
) f4 r8 c8 ( |
) f,4 r8 c'8 ( |
) f4 r8 c8 ( |
) f,2 ~ |
f ( |
) f'4 \fz r^\fermata
}

cb=\notes\relative c {
\clef "bass";

\property Staff."midiInstrument" = "contrabass"

r4 r c-> \mf ~ |
c r c \p ( |
) f,2. ~ |
f ~ |
f2 f'4 ( |
c ) f r |
f,2. ~ |
f ~ |
f ~ |
f2 r4 |
c'2. ~ |
c |
a2 d4 ( |
e a ) f |
c2. \p ~ |
c ~ |
c ~ |
c4 () f r |
e2 f4 ( |
g ) f r |
R2. |
r4 r f ( \< |
\! ) e2 r4 |
r r dis ( \< |
\! ) e2 r4 |
R2.*6 |
r4 r^\fermata r8^\fermata r |
R2*4 |
d8-. \pp r g-. r |
d-. r g-. r |
R2*2 |
c,8-. r f-. r |
c-. r f-. r |
R2 |
r4 r8 f8 \f ~ |
f2 |
f |
f |
d4 () a8 r |
f4.-> f'8 ( |
) e4. r8 |
f,4.-> f'8 ( |
) e4. r8 |
f,8 r f' r |
fis r fis r |
g \fz r r4 |
R2*3 |
r4 g, ( |
) c r |
R2*8 |
c'4_"pizz." \f r |
c, r |
R2*2 |
c'4_"pizz." r |
c, r8 c'8_"arco?" ( |
) c,4.-> c'8 ( |
) c,4. f8-. \p |
[f-. \< f-. f-. \! f-.] |
g4. c,8 ( |
) f,4.-> c'8 ( |
) f,4.-> f'8 |
[f-. \p \< f-. f-. \! f-.] |
g4 r8 c, \p |
c2:8 \< |
c2:8 |
c'2:8 |
\! c4.:8 cis8 \f ( |
) a,4. a8 |
[d e f b ( ] |
) b,4. \f b'8 ( |
) g,4. \f g'8-> |
c,4 c-> ~ |
c c-> ~ |
c c-> ~ |
c r |
c c ~ |
c c ~ |
c c ~ |
c r |
c r |
c r |
c_"dim." r | % dim. added
c r |
c8 r r4 |
R2 |
r8 [c-.^"solo" \p d-. dis-.] |
e r e, r |
a2 \p |
a \< ( |
\! e' \> |
\! ) e4 r |
e2 \< ~ |
\! e4 \> \! r |
e2 \< ~ |
\! e4 \> \! r |
e2 \fz ~ |
e |
f! ( |
[e8 d c ) b]
a2 \p ~ |
a |
e' \fz \> ~ |
\! e4 r |
e2 \> ~ |
\! e4 r |
e2 \> ~ |
\! e4 r |
e2 \fz ~ |
e ( |
) f \p ( |
) e |
d \> ( |
e |
\! f \p |
) e4 r |
R2*6 |
fis8-. r g-. r |
d-. r g-. r |
R2*2 |
c,8-. r f-. r |
c-. r f-. r |
R2 |
r4 r8 f \f ~ |
f2 |
f |
f,4. f'8 |
[e8-. es-. d-. c-. ] |
bes2 |
bes |
bes |
g'4 () d8 r |
bes4. bes'8 ( |
) a4. r8 |
bes,4. bes'8 ( |
) a4. r8 |
bes, r bes' r |
b r b, r |
c \fz r r4 |
R2*3 |
r4 c ( |
) f r |
R2*8 |
f4_"pizz." r  |
f, r |
R2*2 |
f'4 r |
f, r |
f'4._"arco" r8 |
f4. bes,8 \p |
bes2:8 \< |
\! c4. f8 \f ( |
) bes,4.-> <f'8 \p bes ( > |
) bes,4. bes'8 \p | % \p added
c2:8 \< |
\! c4 r8 f, \p |
f2:8 \< |
f2:8 |
\! f2:8 |
f4.:8 fis8 \f ( |
) d4.-> d8 |
[g-. a-. bes-. e ( ] |
) e,4.-> e'8 |
c,4.-> c'8 ( |
) f,4 f~ |
f f~ |
f f~ |
f r |
f, f ~ |
f f ~ |
f f' ~ |
f r |
f r |
f_"dim." r |
f r |
f r |
f2 \p \< ~ |
f ~ |
\! f \> ~ |
\! f |
f, \< ~ |
\! f | 
f'4 ( \f \> cis |
\! d g, |
) c! r |
R2 |
c8 \p r r4 |
c8 r r4 |
f8 \pp r f r |
f r f r |
f r f r |
f r f r |
e r e r |
e r e r |
c4. \f \> d8 |
[e-. f-. \! g-.] r |
f \p r f r |
f r f r |
f r f r |
f r f r |
bes4.-> r8 |
g4.-> r8 |
e!4.-> r8 |
c4. c8 ( |
) f2 ( |
) f,4. c'8 |
f2 ( |
) f,4. d'8 ( |
) g,4.-> g'8 ( |
) c,4.-> c'8 ( |
[ ) f, c-. f-. c-. ] |
[ f-. c-. f-. c-. ] |
f r c r |
f r c' r |
f, r c r |
a r c r |
f4 r8 c8 ( |
) f4 r8 c8 ( |
) f,4 r8 c'8 ( |
) f4 r8 c8 ( |
) f,2 ~ |
f ( |
) f'4 \fz r^\fermata
}
