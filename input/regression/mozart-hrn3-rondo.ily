\version "2.19.21"

\include "mozart-hrn3-defs.ily"

rondotheme = \relative {
  c''8[ c c] c[ c c]
  c4( cis8  d) r g,
  d'8[ d d] d[ d d]
  d4( dis8  e) r c |
  c[( d) e] f[ g a]
  g[ ( e) c] c4 d8
  e4( d8) e4( f8)
  e4.( d8) r r |
}

lipbreaker = \relative {
  r8  g'[-. g-.] c[( e) g,-.]
  c[( e) g,-.] c[( e) g,-.]
  c[ c, c] c[ c c]
  c[ c c] c[ c c]
}

rondo = \relative {
  \partial 8
  \time 6/8
  \key c \major

  g'8\p |

  \rondotheme

  R2.*13 |
  r8 r^\fermata d' d[ e f]
  g[ ( e) c-.] d[( e) d-.]
  c4 c8  d[ e f]
  g[( e) c-.] d[( e) d-.]
  c4 r8 r4 r8 |
  R2.*7
  \mark \default
  c4.\p \acciaccatura e16
  d8[ c d]
  c4 r8 r4 r8
  e4. \acciaccatura g16
  f8[ e f]
  e4 r8 r4 r8
  g4. e4 c8
  g2.~
  8[ a b] c[ d e ]
  e4.( d8) r r
  R2.*4
  e2.~ |
  8[ d c] c[ b a]
  d2.~
  8[ c b] b[ a g]
  g'4( e8) b4( cis8)
  \mark \default
  d4 r8 r4 r8
  R2.*3 |
  r8  d[-. d-.] d[( g) d-.]
  d[( g) d-.] d[ d d]
  d[( g)] r r4 r8
  R2.*1
  \lipbreaker
  c,,4 r8  c'[ d e]
  d4( g8)  c,[ d e]
  d4 r8 r4 r8
  R2. |
  r4 r8  c[-. d-. e-.]
  d4( g8)  c,[ d e]
  d[( g) fis] e[ d c]
  b[ ( e) d] c[ b a]
  \mark \default
  g4 r8 r4 r8
  R2. |
  r8  g[\f g] g[( b) b-.]
  b[( d) d-.] d[( g) g-.]
  g2.~
  8[ \> a g] f[ e d]

  <<
    \rondotheme
    { s8\p }
  >>

  R2.*12
  r4 r8 r4 c8
  \mark \default
  c4 f8 c4 a8
  a4.~4 a8
  bes4 c8 d4 bes8
  g4. ~ 8 r r
  R2.*3
  r4 r8 r4 c8
  a4. c
  f ~  8.[ e16( d c)]
  bes4 g8 e4 g8
  c,4. ~ 8 r r
  R2.*3|
  r4 r8 r4 c'8
  b4( c8) b4( c8)
  bes4. ~ 4 g8
  a4 ( c8) f4 ( b,8)
  d4. ( c8) r r
  R2.*3|
  r4 r8 r4 c8
  b4( c8) b4( c8)
  bes4. ~ 4 g8
  a4 c8  f[ ( d) b!]
  d4. ( c8) r r
  \mark \default
  R2.*9  |
  \lipbreaker
  c,8[ c' c] c4.~
  8[ c d] e[ e fis]
  g4 r8 r4 r8
  R2.
  r8  g,[ g] g[ g g] |
  es'4. ~  8[ d c]
  b4 r8 r4 r8
  R2. |
  r8  g[ g] g[ g g]
  es'4. ~  8[ d c]
  b4.\cresc c4. d4. e4.
  \mark \default
  f2.\f ~ |
  f4 r8 r4 r8
  r8  g,[\> g] g[ g g]

  % Edition breitkopf says a-flat (silly!)
  fis[ g gis]
  a[ bes b]\!

  %% EB does the slur in the Rondo differently from the 1st and 2nd time.
  %% why. Should check with MS.
  <<
    \rondotheme
    { s8\p }
  >>

  R2.*7
  \mark \default
  R2.*4
  c,4.\mf c4 c8
  c4. e4 c8
  g'4. g4 g8
  g4. g,4 g8
  c4 r8 r4 r8
  r4 r8 r4 g'8
  c[ ( e) g,-.] c[ ( e) g,-.]
  c[ ( e) g,-.] c[ ( e) g,-.]
  \mark \default
  g'2.\cresc bes,2.
  a4. b16[ c d e f g]
  a4. f4 d8
  c8[\f g' e] c[ g e]
  c[ e' c]   g[ e c]
  g4 r8 g''8[ e c]
  \afterGrace d2._(\trill { c16[ d] }
  c4) r8 r4 r8
  R2.*5
  r8 r8^\fermata d8\p  d[ e f]
  g[ ( e) c] d[( e) d]
  c[\cresc c c] d[ e f]
  g[( e) c] d[( e) d]
  c4\f  r8 r4 r8
  R2.*5
  c8[\f c, c] c[ c c]
  c4 r8 c4 r8

  % This is technically incorrect, since we started with an 8th
  % note pickup, but both eulenburg and EB do this as well.
  c4 r8 r4 r8 \bar "|."
}

