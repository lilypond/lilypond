\header{
filename =	"violoncello-ii.ly";
title =		"Vier Duette";
description =	"Four duets for Violino and Violoncello (Viola)";
opus = 		"BWV";
composer =	"Johann Sebastian Bach (1685-1750)";
enteredby =	"jcn";
copyright =	"Public Domain";
}

\version "1.3.42";

$violoncello_ii = \notes\relative c{
  r2 a'2|c e,| f8(e)d2 e8()fis| g a bes2 a8()gis|
%5
  a(e)a b a(e)a c|b(e,b')c b(e,b')d|c(e,c')d c(e,c')e|b(e,b')c b(e,b')d|
  c(b)a2 gis4|
%10
  a8()c b d c()b c a|e4 fis g fis8()e|fis4 gis a2~|a4 g8(fis)g4 e~|
  e dis8 cis dis2\prall|
%15
  e4 b' e2~|e dis\prall|e4 e,8()fis g a bes g|e f g4. f8(e)d|cis d e4.d8(cis)b|
%20
  cis()a cis d e()f g e|f()e d e f()g as f|d e f4. e8(d)c|b c d4. c8(b)a|
  b()g b c d()e f d|
%25
  e()d c d e()f g e|f4 a()d, f(|)b, c8 d e4 c(|)d f()b, d(|)a a8 b c4 a(|
%30
  )b d()gis, b(|)e, fis8()gis a4 b|c2 e,|f8 e d4~d8 e(fis)gis|a e a2 gis4|
%35
  a b c b8()a|b4 c d2~|d4 c8 b c4 a~|a gis8 gis gis2\prall|a4 e' a2~|
%40
  a gis\prall|a e|f a,|c8(b)a2 b8 cis|d()e f2 e8()dis|
%45
  e(b e)fis e(b e)g|fis(b,fis')g fis(b,fis')a|g(b,g')a g(b,g')b|
  fis(b,fis')g fis(b,fis')a|g(fis)e2 fis4|
%50
  gis f'!(e\prall)d |f, d'(c\prall)b|d, b'(a\prall)gis|a8 d,(c)b a4 g'|
  fis es'(d\prall)c|
%55
  es, c'(b\prall)a|c, a'(g\prall)fis|g8 c,(b)a g4 f'(|)e8 e'(c)b c()a e4~|
  e8 d e fis d()g b4|
%60
  c,8(b'a)g a()fis c4~|c8 b c d b()d g4|a,8(g'fis)e fis()dis a4~|
  a8 g a b g d' e g|fis(ais,fis')g fis(b,fis')g|
%65
  gis(e gis)ais b(e,g)b|a(cis,a')bes a(d,a')c|b(g c)cis d f,(bes)d|
  c e,(c')d c f,(a)c|b d,(b')c b e,(b')d|
%70
  c a(c)e c a()b gis(|)a c(b)d c b(c)a|e4 fis g fis8()e|fis4 gis a2~|
  a4 g8 fis g4 e~|
%75
  e dis8 cis dis2\prall|e4 b' e2~|e dis\prall|e r4 d|cis bes(a\prall)g|
%80
  bes, g'(()f\prall)e|g, e'(()d\prall)cis|d8()a d e f g as f|d e f4.(e8 d)c|
  b c d4.(c8 b)a|
%85
  b()g b c d()e b d|e4 e, a2~|a4 g8 fis g2~|g4 a8 b c2~|c4 bes8 a bes2~|
%90
  bes4 c8 d es2~|es4 d8 cis d2~|d cis\prall|d4 e f d|e d' c b|
%95
  a8()e a2 gis4\prall| a2 b|c e,|c8(e)d2 e8()fis|g a bes2 a8()gis|
%100
  a(e a)b a(e a)c| b(e,b')c b(e,b')d|c(e,c')d c(e,c')e|
  b(e,b')c b(e,b')d| c(a b)cis d()e f g|
%105
  f e d cis d4 d,~|d c8 b c4 d|e d e e,|<e'1 a,>
  \bar "|.";
}

\include "global-ii.ly"

$violoncello_ii_staff = \context Staff = violoncello <
  \property Staff.instrument = "cello"
  \$violoncello_ii
  \clef bass;
  \$global_ii
>
