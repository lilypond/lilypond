\header{
filename =	"violino-ii.ly";
title =		 "Vier Duette";
description =	"Four duets for Violino and Violoncello (Viola)";
opus =		"BWV";
composer =	"Johann Sebastian Bach (1685-1750)";
enteredby =	"jcn";
copyright =	"Public Domain";
}

\version "1.1.52";

$violino_ii = \notes\relative c''{
  R1*8 |
  r2 e | 
%10
  fis a,|c8(b)a2 b8()cis|d e f2 e8()dis|e(b e)fis e(b e)g|
  fis(b,fis')g fis(b,fis')a|
%15
  g(b,g')a g(b,g')b|fis(b,fis')g fis(b,fis')a|g fis e2 d4|cis bes'(a\prall)g|
  bes, g'(f\prall)e|
%20
  g, e'(d\prall)cis|d8 g,(f)e d4 c'|b as'(g\prall)f|as, f'(e\prall)d|
  f, d'(c\prall)b|
%25
  c8 f,(e)d c4 bes'(|)a8 g'(f)e f d a4~|a8 g(a)b g c e4| f,8(e'd)c d b f4~|
  f8 e(f)g e g c4|
%30
  d,8(c'b)a b gis d4|d8 c(d)e c f e d|e4 a,2()gis4|a2 a'|c e,|
%35
  f8(e)d2 e8()fis|g a bes2 a8()gis|a(e a)b a(e a)c|b(e,b')c b(e,b')d|
  c(e,c')d c(e,c')d|
%40
  b(e,b')c b(e,b')d| c(b)a2 gis4|a8()c b d c()b c a|e4 fis g fis8()e|fis4 gis a2~
%45
  a4 g8 fis g4 e~|e dis8 cis dis2\prall|e4 b' e2|e dis\prall|e4. d16(e f8)e d c|
%50
  b c d4. c8(b)a|gis a b4. a8(g)fis|gis()e gis a b()c d b|c()b a b c()d es c|
  a b c4. b8(a)g|
%55
  fis g a4.g8(fis)e| fis()d fis g a()b c a|b()a g a b()c d b|c4 e()a, c(|
  )fis, g8 a b4 g(|
%60
  )a c()fis, a(|)d, e8 fis g4 e(|)fis a()dis, fis(|)b, cis8 dis e2~|
  e4 d8 cis d2~|
%65
  d4 e8 fis g2(|)g4 f8 e f2~|f4 g8 a bes2~|bes4 a8 gis a2~|
  a gis\prall|
%70
  a e'|f a,|c8(b)a2 b8()cis|d e f2 e8()dis|e(b e)fis e(c)e g|
%75
  fis(b,fis')g fis(b,fis')a|g(b,g')a g(b,g')b|fis(b,fis')g fis(b,fis')a|
  g\prall b,(e)fis g()a b()g|e f g4. f8(e)d|
%80
  cis d e4. d8(c)b|cis a cis d e()f g()e|f e d cis d4 c|b as'(g\prall)f|
  as, f'(e\prall)d|
%85
  f, d'(c\prall)b| c8(g)c d c fis,(a)c|b(dis,)b' c b f,(b')d|
  cis(a)cis dis e g,(c)e|d(f,)d' es d g,(d')f|
%90
  e(c)e fis g bes,(es)g|f(a,)f' g f bes,(d)f|e(gis,)e' f e a,(e')g|
  f()g f e d()c b a|gis e()fis()g e gis a b|
%95
  c2 e,|f8 e(d)cis d e fis gis|a e a2 gis4(|)a b()c b8 a|b4()cis <d2 d,>~|
%100
  <d4 d,> c8 b c4 <a a,>~|<a a,> gis8 fis g2\prall|
  <a2 e a,> <e' a,> <a a,> <a c,>|
  %<a2 b, d4><gis\prall b,4 d>|
  \context Staff<
    \context Voice { 
      \property Voice.verticalDirection=1
      <a2 b,>gis\prall|a8 g f4~f8 e(d)cis|
%105
      d c bes4~bes8 a(gis)fis|gis4 e <e' a,><f a,>|
      %c2 b()\grace a|
      c2 b|
      a1|
    }
    \context Voice=x { 
      \property Voice.verticalDirection=-1
      d,4 s<b4 d>s|<a2 d> s2|
%105
      <d g,> s2|s1|
      %e2 d ()\grace c|
      e2 d|
      c1|
    }
  >
  \bar "|.";
}

\include "global-ii.ly"

$violino_ii_staff = \context Staff = violino <
  %urg
  % \notes\property Voice.textStyle = "large" s4^"Moderato"
  % \notes {s4. \property Voice.textStyle = "large" s4^"Moderato"}
  % urg, timidity violino patches broken?
  %\property Staff.instrument = "violin"
  \property Staff.instrument = "viola"
  \$violino_ii
  \$global_ii
>
\version "1.1.52";
