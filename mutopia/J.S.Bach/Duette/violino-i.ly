\header{
filename = 	"violino-i.ly";
title = 		 "Vier Duette";
description = 	"Four duets for Violino and Violoncello (Viola)";
opus = 		"BWV";
composer = 	"Johann Sebastian Bach (1685-1750)";
enteredby = 	"jcn";
copyright = 	"Public Domain";
}

\version "1.3.122";

violinoI =  \notes\relative c'' {
  \property Voice.autoBeamSettings \override #'(end * * * *) = #(make-moment 3 8)\property Voice.autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 1 16)
  %{
     should fix autobeamer: not only check shortest type's end in beam,
     but also the newly added type's end
  
     Normally, we want

         [c8 c c] and [c16 c  c c  c c],

     ie '(end *) = 3/8

     However, we don't want

         [c16 c  c c  c8] c8,  TODO: manually correct these

     but rather

         [c16 c  c c]  c8 c8,

     ie '(end X) = 1/4
	 
    X can't be 1/16 or 1/8

    Hmm.
   %}
   
  r4 r8 g16(fis e)d b'8 a r r g16(fis e)d c'8 |
  b r r d16(b)a g(e')c d(b)a g(c)a c(b)a g(a)fis |
  g d(e fis g a )b d(c b c a )b g(a b c d )e g()fis e(d)cis |
  a'd,(e fis g a )b a(g fis g e )fis a(g fis e )d 
  %%cis d e fis <g8 { \grace a, } { \grace d, } >~|
  cis d e fis <g8 a, d,>~|
%5
  %%<\grace a,16 \grace d, g'16 > e(fis)d cis d 
  <g16 a, d,> e(fis)d cis d 
  g,8 <f'4 g, g,>~f16 d(e)c b c fis,!8 <e'4 fis,>~|
  <e16 fis,> c(d)b a b e, b'(c)a g a d, a'(b)g fis g c, g'(a)fis e fis |
  b,(d)e fis(g)a b(d)c b(a)g fis(g)a b(c)d e(a)g fis e d |
  g d(e)fis g a b g(fis)g c, a' b, g'(fis)g a, c fis, g(a)b <c8( d, a>~|
  <c d, a> )b r d16(b a)g e'8 d r r c16(b a)g g'8 | 
%10
  <fis a, d,> r r g16(fis)e d(b')g a(fis)e d(g)e g(fis)e d(e)cis |
  <d4. d,>~<d16 d,> fis,(g e d)e <c'!4. d,>~<c16 d,> e,(fis d cis)d |
  <b'4. g g,>~<b16 g g,> e,(dis)e fis g a8 b <c fis,>~ <c16 fis,> b(a)g fis e |
  dis e fis g <a8 fis>~<a16 fis> fis(g)e d e b e(g)e dis e b e(g)e d e |
  cis d e fis <g8 a,>~<g16 a,> e(fis)d cis d a d(fis)d cis d a d(fis)d c d |
%15
  b c d e <f8 g,>~<f16 g,> e(dis)e a e cis' a(gis)a dis a fis'8 [b,()a] |
  g16 b(cis dis e fis )g b( a g fis e)dis cis( dis e fis g)a fis(e dis cis b |
  )e g(fis)e a fis g b(a)g a fis g b(a)g fis e [dis()e fis()c] <a'8( b, fis b,>~|
  <a b,> <)g b,> r [b,16(a g)fis] c'8 b r r [a16(g fis)e] e'8 |
  dis8 r r e16 d(cis b g' e)fis d(cis b e cis)e d(cis b cis)ais |
%20
  <b4. d, g>~<b16 d,> ais( b d cis)e d8 [b()cis\prall] d [gis()b] |
  <d,4. e,>~<d16 e,> gis,(a c! b)d <c8 e,> [a()b\prall] c [fis()a]|
  <c,4. d,>~<c16 d,> a(b)g fis g e b'(c)a g a fis c'(d)b a b |
  g d'(e)c b c a e'(f)d c d b8 <g'4 g, g,>~<g16 g,>c,(f! e d c |
  )b c(f, a g f)e g(f e d c )g c(d e f g )a d(c)b a g |
%25
  c16 g a b c d e g(f)e f d e  c'(b)c a f g  c,(b)c <f8 g, g,>~|
  <f16 g,> d(e)c b c fis8 a <c d, d,>~<c16 d, d,> c,(d)b a b e,8 <d'4 e,> ~|
  <d16 e,> b(c)a gis a e' b(c)a g a fis' c(d)b a b g' d(e)c b c |
  a' g(fis e d c)b d(c b a g)fis e'(d c b)a \breathe g(fis e)d b'8|
  a r r r16 a, b c d(e)fis g a b c d g,(fis e)d c'8 | 
%30
  b r r r4 r8 r4 r8 d16 b(a g e')c|
  d b(a g c a)b d(c f c a)b g(fis g a fis)g b(a g a fis|
  )g e(d)c f d e g(f)e f d e f(g)a d, f e a(b)c e, g|
  fis g(a)b e, g fis b(c)d fis, a g a(b)c fis, a g c(d)e g, b|
  a g(a b c)d c(b a)g <f'8 g, g,> <e g, g,> r r d16(c b)a <g'8 a, d,> |
%35
  <fis a, d,> r r a16( fis e)d <c'8 a, d,> <b16 b, d, g,> g(a)b e,(g)c,
  fis(g)a d,(fis|
  )b, d(e)f b,(d)g, b(c)d g,(b)d, g(a)b d,(g)b, d(e)f b,(d|
  )g,8 r r e'16(d c)b f'8 e r r g16(fis e)d c'8 |
  <c4.\prall>~c16 b(a g)e' c()d b(a g)c a()c b(a)g
  \context Staff<
    %\context Voice { a16 f | g1. }
    \context Voice { \stemUp\slurUp\tieUp a16 fis | g1. }
    \context Voice=x { \stemDown\slurDown\tieDown c,16 c | b1. }
  >
  \bar "|.";
}

\include "global-i.ly"

violinoIStaff =  \context Staff = violino <
  %urg
  % \notes\property Voice.TextScript \set #'font-style = #'large s4^"Moderato"
  % \notes {s4. \property Voice.TextScript \set #'font-style = #'large s4^"Moderato"}
  % urg, timidity violino patches broken?
  %\property Staff.instrument = "violin"
  \property Staff.instrument = "viola"
  \violinoI
  \globalI
>
