\header{
filename = 	"violoncello-i.ly"
title = 		"Vier Duette"
description = 	"Four duets for Violino and Violoncello (Viola)"
opus =  		"BWV"
composer = 	"Johann Sebastian Bach (1685-1750)"
enteredby = 	"jcn"
copyright = 	"Public Domain"
}

\version "1.3.146"

violoncelloI =  \notes\relative c{
  \property Voice.autoBeamSettings \override #'(end * * * *) = #(make-moment 3 8)
  % too many [c8 c c] stuff here some manual beaming
  %\property Voice.autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 1 16)

  g8()a d, g b()g d()fis a d a()fis |
  g()d b' g b()c b()e fis g d()d, |
  g r r [d'16(b a )g] e'8 d r r [c16(b a )b] g'8 |
  fis r r g16 fis( e d b' g )a fis( e d g)e g fis( e d e)cis |
%5
  d8 d'()a b16 c(d)b g b c,8 c'()g a16 b(c)a fis a |
  b,8()b' fis g a()e fis g()d e fis()d |
  g r r [g,16(fis e)d] b'8 a r r [g16(fis e )d] c'8 |
  b r r d16 b(a)g e' c d b(a)g c a c b(a)g a fis |
  g d( e fis g a )b d(c b c )a b g(a b c b )e g( fis e d cis |
%10
  )a'16 d,( e fis g a)b a(g fis g e)fis a(g fis e d)cis d(e)fis g8~ |
  g16 e(fis)d cis d b'4.~b16 d,(e)c b c a'4.~|
  a16 fis(g)e dis e c'4.~c16 b(a)g fis e dis8(e)c |
  b b'16(a g)fis e8 e,()fis g()b a g e()e' |
  a,16 b(cis)d e cis d8 d,()e fis a()g fis d()d' |
%15
  g,16(a b)c! d b c8 c'()fis, a dis,()fis b,16 cis(dis e)fis d |
  e8 e, r [e'16( dis cis)b] g'8 fis r r [e16(dis cis)b] a'8 |
  g r r b16 g(fis)e c'! a b g(fis)e a fis a g(fis)e fis dis |
  e b(cis dis)e fis g b(a)g a fis g e(fis)g a b cis e(dis)c b ais |
  fis b,(cis)dis e fis g fis(e)d e cis d8()d, g' e fis()fis,|
%20
  b16 fis'(gis)ais b cis d cis(d)b ais b fis b(d)b ais b f b(d)b a b |
  e, b'(e)d c b a b(c)a gis a e a(c)a gis a es a(c)a g a|
  d, a'(d)c b a g8()g, b c a()c d()b d |
  e c()e f g, f'~f16 e(f b, a)b e8()a f |
  g g, r [c16(b a)g] e'8 d r r [c16(b a)g] f'8|
%25
  e r r g16 e(d)c a' f g e(d)c f d f e(d)c d b |
  c8 c'()g a16 b(c)a g fis b8 b,()fis' gis16 a(b)g fis e |
  a,8()a' b c b()c d()b d e [c()a]|
  fis()d r [g16(fis e)d] b'8 a r r b16 d(c)b a g |
  e e'(d)c b a [g(fis e)d] c'8 b r r r16 b, c d e fis |
%30
  g a(b c d)e d b(a g e')c d b(a g c)a b d(c b c)a|
  b g(fis e a fis)g b(a g a fis)g b(a g c a)b g(fis e fis dis|
  )e g(f)e a fis g e(d)c d c c8()c' b c a()c |
  d,()d' cis d b()d e,()e' dis e c()e |
  fis,()e d g [c,16(b a)g] c g'(a c b)d e8 [d,16(cis b)a]|
%35
  d a'(b cis d)e fis8 [g,16( fis e)d] g8()e c a d()d,|
  g r r r16 d''(e)f b,(d)g, b(c)d g,(b)d, f(g)a d,(f|
  )b, d(e)f b,(d)g,8 a()b c16 d(e f g a)b d(c b a)g|
  % FIXME?
  fis [a(g)fis e d] g8()e c g'8 b, e c d d, |
  g1.|
  \bar "|."
}

\include "global-i.ly"

violoncelloIStaff =  \context Staff = violoncello <
  \property Staff.instrument = "cello"
  \violoncelloI
  \clef bass
  \globalI
>
