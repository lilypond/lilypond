\header{
title =	 "La Gallina a 2 violini";
date =	 "1637";
source =	 "the manuscript of Canzoni overo Sonate Concertate "
	 "libro terzo, opera duodecima ";
composer =	 "Tarquinio Merula (1594/95-1665)";
enteredby =	 "Mats Bengtsson";
copyright =	 "Public Domain ";
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

\version "0.1.9";

vi1=\melodic{
  \meter 4/4;
  \octave c'';

  [d8 d d d] d4 [A16 B c A] |
  [B8 G ][ G G16 A][ B8 G ][ G G16 A] | 
  [B8 G ][ G A16 B] c4 B |
  [A8 D] G2 Fis4 |
  G2 r2 |
  r1 |
  [d8 d d d] d4 [A16 B c A] |
  [B8 G ][ G G16 A][ B8 G ][ G G16 A] | 
  [B8 G ][ G A16 B][ c8 e d c] |
  [B G] c2 B4 |
  c2 r |
  r1 |
  [g8 g g g] g4 [d16 e f d] |
  [e8 c ][ c c16 d][ e8 c ][ c c16 d] |
  [e8 c ][ c d16 e] f4 e |
  [d8 G] c2 B4 |
  [c8 G ][ G G16 A] B4 G |
  r8 G [G G16 A] B4 G |
  r8 [E16 Fis ][ G8 G] [ Fis! G G Fis!] |
  G2 r |
  r1 |
  r16 [d c B ][ A G Fis E] [D8 d ][ d d16 c] |
  [B8 d ][ d d16 c][ B8 d ][ d d16 e] |
  fis4 g2 fis!4 |
  r16 [g f e ][ d c B A][ G8 g ][ g g16 f] |
  [e8 g ][ g g16 f][ e8 g ][ g g16 a] |
  b4 c'2 b4 |
  [c'8 g ][ g g16 f] e4 d |
  r8 g [g g16 f] e4 d |
  r8 [d16 A ][ B8 B16 c] A2 |
  B1 |
  \meter 3/2;
  \tempo 2=140;
  r4 d d d d d |
  e1. |
  r4 c c c c c |
  d1. |
  r4 [d8 c] B4 [c8 d] G4 [A8 B] |
  E1. |
  r4 [e8 d] cis4 [d8 e] A4[ B8 cis!] |
  Fis1. |
  r4 d d d d d |
  e [e8 d] c4 [d8 e] A4 [B8 c] |
  Fis4 [fis8 e] d4 [e8 fis!] B4 [c8 d] |
  G4 g g g g g |
  a4 [c'8 b] a4 [b8 c'] f4 [g8 a] |
  d4 g g g g g |
  a [d8 c] B4 [c8 d] A2 |
  \meter 4/4;
  \tempo 4=80;
  B1 |
  [d8 d d d] d4 [A16 B ][ c A] |
  [B8 G ][ G G16 A] [B8 G ][ G G16 A] | 
  [B8 G]  c2 B4 |
  c2 r |
  [g8 g g g] g4 [d16 e f d] |
  [e8 G ][ G G16 A] [B8 d ][ d d16 e] |
  fis4 g2 fis!4 |
  r16 [g f e ][ d c B A] [G8 g ][ g g16 f] |
  e2 r8 d [d d16 e] |
  [fis a g fis ][ e d c B] [A8 d ][ d d16 e] |
  fis4 g2 fis!4 |
  \cadenza 1;
  g\breve
  \bar "|.";
}

vi2=\melodic{
  \meter 4/4;
  \octave c'';

  r1 | r | r | 
  [d8 d d d] d4 [A16 B c A] |
  [B8 G ][ G G16 A] [B8 G ][ G G16 A] | 
  [B8 G ][ G A16 B] c4 B |
  [A8 D] G2 Fis4 |
  G2 r2 |
  r1 |
  [g8 g g g] g4 [d16 e f d] |
  [e8 c ][ c c16 d] [e8 c ][ c c16 d] |
  [e8 c ][ c d16 e] f4 e |
  [d8 G] c2 B4 |
  c2 r |
  r1 |
  [g8 g g g] g4 [d16 e f d] |
  e2 r8 G [G G16 A] |
  B4 G r8 G [G A16 B] | 
  c4 B A2 |
  G r16 [d c B ][ A G Fis E] |
  [D8 d ][ d d16 c] [B8 d ][ d d16 e] |
  fis4 g g4. fis!8 |
  g2 r |
  r16 [d c B ][ A G Fis E] [D8 d ][ d d16 c] |
  B4 c2 B4 |
  c d G2 |
  r16 [g f e ][ d c B A] [G8 g ][ g g16 f] |
  e4 d r8 g [g g16 f] |
  e4 d r8 [c16 G ][ B8 B16 c] |
  A4 G2 Fis4 |
  G1 |
  \meter 3/2;
  r1. |
  r4 [g8 f] e4 [f8 g] c4 [d8 e] |
  A1. |
  r4 [a8 g] fis4 [g8 a] d4 [e8 fis!] |
  B1. |
  r4 g g g g g |
  a1. |
  r4 a a a a a |
  b [d8 c] B4 [c8 d] G4 [A8 B] |
  E4 [g8 f] e4 [f8 g] c4 [d8 e] |
  A4 [a8 g] fis4 [g8 a] d4 [e8 fis!] |
  B4 [b8 a] g4 [a8 b] e4 [fis8 g] |
  c4 A A A A A |
  B [d8 c] B4 [c8 d] D4 E |
  Fis2 G Fis! |
  \meter 4/4;
  G1 |
  r1 | r1 |
  [g8 g g g] g4 [d16 e f d] |
  [e8 c ][ c c16 d] [e8 G ][ G G16 A] |
  [B8 G] c2 B4 |
  c2 r |
  r16 [d c B ][ A G Fis E] [D8 d ][ d d16 c] |
  B4 c2 B4 |
  [c8 G ][ G G16 A] [B d c B ][ A G Fis E] |
  [D8 d ][ d d16 e] [fis16 a g fis ][ e d c B] |
  [A d c B ][ A G Fis E] D4 d |
  \cadenza 1;
  B\breve
  \bar "|.";
}

bc=\melodic{
  \clef "bass";
  \meter 4/4;
  \octave c;

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
  \meter 3/2;
  g1. | c |
  f | d^"\\textsharp" |
  g | c |
  A | d^"\\textsharp" |
  G | c |
  d^"\\textsharp" | e |
  f | g1 B2 |
  d1.^"3 4 3" |
  \meter 4/4;
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
  \type Staff_group <
    \vi1
    \vi2
    \bc
  >
  \paper{
    %linewidth=18.\cm;
     %\output "gallina.out";
    gourlay_maxmeasures=7.;
%%%% Uncomment to get the original layout without beams.
%%%% Compare to the definition in init/engraver.ly.
%    Voice = \translator {
%       \type "Engraver_group_engraver";
%       \consists "Dynamic_engraver";
%       \consists "Rest_engraver";
%       \consists "Stem_engraver";
%       \consists "Plet_engraver";
% %     \consists "Beam_engraver";
%       \consists "Beam_req_swallow_translator";
%       \consists "Abbreviation_beam_engraver";
%       \consists "Script_engraver";
%       \consists "Rhythmic_column_engraver";
%       \consists "Slur_engraver";
%       \accepts "Thread";
%    }
  }
  \midi{ 
        \tempo 4=80;
  }
}
