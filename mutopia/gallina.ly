\header{
title =	 "La Gallina à 2 violini";
date =	 "1637";
source =	 "the manuscript of Canzoni overo Sonate Concertate "
	+ "libro terzo, opera duodecima ";
composer =	 "Tarquinio Merula (1594/95-1665)";
enteredby =	 "Mats Bengtsson";
copyright =	 "Public Domain ";
subtitle = "from Canzoni overo Sonate Concertate libro terzo, "
	+ "opera duodecima 1637";
}


%{
From mats.bengtsson@s3.kth.seThu Aug 14 02:11:19 1997
Date: Wed, 13 Aug 1997 18:24:53 +0200
From: Mats Bengtsson <mats.bengtsson@s3.kth.se>
To: Han-Wen Nienhuys <hanwen@stack.nl>
Subject: Re: brevis? 

[snip]

Hopefully it could be useful input to future improvements. I've entered the
music directly from the 1637 manuscript and intend to publish it public
domain on for example ftp.gmd.de when finished. You could include it as an
example in the distribution, but in that case I could add a LaTeX title file.
There were no beams in the manuscript so I didn't use them in this edition 
either.

      /Mats


Tested Features: Note placement, multipart score, figured base, \breve

%}
%{

note: the sharp signs hoovering over the bass part are no mistake, but
part of the basso continuo --HWN

Note also: The original score was printed without any beams, 
they were added in this edition to increase the readability. 
If you want the original beam-less layout, redefine the
Voice engraver by uncommenting the lines in the paper
definition below. --MB
%} 

\version "1.0.7";

vi1=\notes \relative c'' {
  \time 4/4;

  [d8 d d d] d4 [a16 b c a] |
  [b8 g ][ g g16 a][ b8 g ][ g g16 a] | 
  [b8 g ][ g a16 b] c4 b |
  [a8 d,] g2 fis4 |
  g2 r2 |
  r1 |
  [d'8 d d d] d4 [a16 b c a] |
  [b8 g ][ g g16 a][ b8 g ][ g g16 a] | 
  [b8 g ][ g a16 b][ c8 e d c] |
  [b g] c2 b4 |
  c2 r |
  r1 |
  [g'8 g g g] g4 [d16 e f d] |
  [e8 c ][ c c16 d][ e8 c ][ c c16 d] |
  [e8 c ][ c d16 e] f4 e |
  [d8 g,] c2 b4 |
  [c8 g ][ g g16 a] b4 g |
  r8 g [g g16 a] b4 g |
  r8 [e16 fis ][ g8 g] [ fis! g g fis!] |
  g2 r |
  r1 |
  r16 [d' c b ][ a g fis e] [d8 d' ][ d d16 c] |
  [b8 d ][ d d16 c][ b8 d ][ d d16 e] |
  fis4 g2 fis!4 |
  r16 [g f e ][ d c b a][ g8 g' ][ g g16 f] |
  [e8 g ][ g g16 f][ e8 g ][ g g16 a] |
  b4 c2 b4 |
  [c8 g ][ g g16 f] e4 d |
  r8 g [g g16 f] e4 d |
  r8 [d16 a ][ b8 b16 c] a2 |
  b1 |
  \time 3/2;
  \tempo 2=140;
  r4 d d d d d |
  e1. |
  r4 c c c c c |
  d1. |
  r4 [d8 c] b4 [c8 d] g,4 [a8 b] |
  e,1. |
  r4 [e'8 d] cis4 [d8 e] a,4[ b8 cis!] |
  fis,1. |
  r4 d' d d d d |
  e [e8 d] c4 [d8 e] a,4 [b8 c] |
  fis,4 [fis'8 e] d4 [e8 fis!] b,4 [c8 d] |
  g,4 g' g g g g |
  a4 [c8 b] a4 [b8 c] f,4 [g8 a] |
  d,4 g g g g g |
  a [d,8 c] b4 [c8 d] a2 |
  \time 4/4;
  \tempo 4=80;
  b1 |
  [d8 d d d] d4 [a16 b c a] |
  [b8 g ][ g g16 a] [b8 g ][ g g16 a] | 
  [b8 g]  c2 b4 |
  c2 r |
  [g'8 g g g] g4 [d16 e f d] |
  [e8 g, ][ g g16 a] [b8 d ][ d d16 e] |
  fis4 g2 fis!4 |
  r16 [g f e ][ d c b a] [g8 g' ][ g g16 f] |
  e2 r8 d [d d16 e] |
  [fis a g fis ][ e d c b] [a8 d ][ d d16 e] |
  fis4 g2 fis!4 |
  \cadenza 1;
  g\breve
  \bar "|.";
}

vi2=\notes \relative c'' {
  \time 4/4;

  r1 | r | r | 
  [d8 d d d] d4 [a16 b c a] |
  [b8 g ][ g g16 a] [b8 g ][ g g16 a] | 
  [b8 g ][ g a16 b] c4 b |
  [a8 d,] g2 fis4 |
  g2 r2 |
  r1 |
  [g'8 g g g] g4 [d16 e f d] |
  [e8 c ][ c c16 d] [e8 c ][ c c16 d] |
  [e8 c ][ c d16 e] f4 e |
  [d8 g,] c2 b4 |
  c2 r |
  r1 |
  [g'8 g g g] g4 [d16 e f d] |
  e2 r8 g, [g g16 a] |
  b4 g r8 g [g a16 b] | 
  c4 b a2 |
  g r16 [d' c b ][ a g fis e] |
  [d8 d' ][ d d16 c] [b8 d ][ d d16 e] |
  fis4 g g4. fis!8 |
  g2 r |
  r16 [d c b ][ a g fis e] [d8 d' ][ d d16 c] |
  b4 c2 b4 |
  c d g,2 |
  r16 [g' f e ][ d c b a] [g8 g' ][ g g16 f] |
  e4 d r8 g [g g16 f] |
  e4 d r8 [c16 g ][ b8 b16 c] |
  a4 g2 fis4 |
  g1 |
  \time 3/2;
  r1. |
  r4 [g'8 f] e4 [f8 g] c,4 [d8 e] |
  a,1. |
  r4 [a'8 g] fis4 [g8 a] d,4 [e8 fis!] |
  b,1. |
  r4 g' g g g g |
  a1. |
  r4 a a a a a |
  b [d,8 c] b4 [c8 d] g,4 [a8 b] |
  e,4 [g'8 f] e4 [f8 g] c,4 [d8 e] |
  a,4 [a'8 g] fis4 [g8 a] d,4 [e8 fis!] |
  b,4 [b'8 a] g4 [a8 b] e,4 [fis8 g] |
  c,4 a a a a a |
  b [d8 c] b4 [c8 d] d,4 e |
  fis2 g fis! |
  \time 4/4;
  g1 |
  r1 | r1 |
  [g'8 g g g] g4 [d16 e f d] |
  [e8 c ][ c c16 d] [e8 g, ][ g g16 a] |
  [b8 g] c2 b4 |
  c2 r |
  r16 [d c b ][ a g fis e] [d8 d' ][ d d16 c] |
  b4 c2 b4 |
  [c8 g ][ g g16 a] [b d c b ][ a g fis e] |
  [d8 d' ][ d d16 e] [fis16 a g fis ][ e d c b] |
  [a d c b ][ a g fis e] d4 d' |
  \cadenza 1;
  b\breve
  \bar "|.";
}


bc=\notes\transpose c'{
  \clef "bass";
  \time 4/4;

  G2 d^"4 3" | G1 |
  g2 c4 G | d1^"3 4 3" |
  G1 | g2 c4 G |
  d1^"3 4 3" | G |
  g2 c4 d | G1^"3 4 3" |
  c1 | c2 F4 c | 
  G1^"3 4 3" | c |
  c2 F4 c | G1^"3 4 3" |
  c2 G | g G |
  c4 G d2^"3 4 3" | G g |
  fis g | d1^"3 4 3" |
  G2 B | d1^"3 4 3" |
  G^"3 4 3" | c4 B c2 |
  G1^"3 4 3" | c4 B c g |
  c B c G | d1^"3 4 3" |
  G1 |
  \time 3/2;
  g1. | c |
  f | d^"\\textsharp" |
  g | c |
  A | d^"\\textsharp" |
  G | c |
  d^"\\textsharp" | e |
  f | g1 B2 |
  d1.^"3 4 3" |
  \time 4/4;
  G1 |
  g2 fis | g G |
  g1^"3 4 3" | c |
  G^"3 4 3" | c2 G |
  d1^"3 4 3" | G^"3 4 3" |
  c2 G | d1^"\\textsharp" ( |
  ) d^"3 4 3" | 
  \cadenza 1;
  G\breve 
  \bar "|.";
}

\score{
  \type StaffGroup <
    \property StaffGroup.timeSignatureStyle = "old"
    \vi1
    \vi2
    \bc
  >
  \paper{
    gourlay_maxmeasures=7.;

    \translator { \VoiceContext

%% Uncomment to get the original layout without beams.
%%%% Compare to the definition in init/engraver.ly.

%  \remove "Beam_engraver";
 
 }
    
  }
  \midi{ 
        \tempo 4=80;
  }
}
