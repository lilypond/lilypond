	%{
From: bf250@freenet.carleton.ca (John Sankey)
To: hanwen@cs.uu.nl, jantien@xs4all.nl
Subject: the first Sonata
Date: Sat, 16 Jan 1999 20:50:07 -0500 (EST)

To give you an idea of what is involved in the Scarlatti project,
here is the Mudela for the first sonata of the 550, organized so I
can proofread the ps output against the original manuscripts. Lily
mutters about clashing note columns and spans, but puts everything 
except note stems where they should be.

I've set it on a double staff to separate the parts enough so they
are readable without setting all the note stem directions manually.
Voices 1 & 3 are the chords, 2 & 4 the notes that can't be chorded
because of duration.

I have a program that does most of the enharmonic corrections to
mi2mu output properly. I'm mulling over how to insert beam brackets
(so I can correct the few it gets wrong) and set stem directions
(ditto) mostly automatically too, but that's not as easy.

Suggestions welcome as always
John

%}

\score {
\header {title ="Domenico Scarlatti - Sonata K.1 L.366";
          description="Allegro";}
% vOne is right hand outside part, vTwo r.h. inside part,
% vThree left hand outside, vFour l.h. inside.
\notes {
\context Staff = staffOne <
\property Staff.nolines=11
\keysignature bes;
\time 4/4;
\clef alto;
\context Voice = vOne <
{
% 1
\stemup [d''16 e'' f'' g''] [a'' a' cis'' a'] d''4. e''8 |
% 2
[f''16 d'' g'' e''] [a'' f'' e'' d''] [cis''8 a''] a''4^\prall~
% 3
[a''16 g'' f'' e''] [d'' c'' bes' a'] bes'8  \stemdown  bes' [g'
bes'] |
% 4
[e'8 e'] [e' e'] f \stemup f'' f''4^\prall~
% 5
[f''16 e'' d'' c''] [bes' a' g' f'] g'8 \stemdown   g' [e' e'] |
% 6
[e'8 a] [a a] [d e] [f g] |
% 7
a16 s b s cis' s d' s e' s f' s g' s a' s |
% 8
b'16 s cis'' s d'' s e'' s [f'' d'' bes' g'] [e'' cis'' a' cis'']
|
% 9
\stemup a''4 g''4^\prallmordent \stemdown [f''16 d'' bes' g']
[e'' cis'' a' cis''] |
% 10
\stemup [f''16 d'' g'' e''] [a'' f'' e'' d''] [cis'' a' d'' b']
[e'' g' f' e'] |
% 11
[f'16 d' g' e'] [a' f' e' d'] [cis' a d' b] [e' cis' f' d'] |
% 12
g'16 \stemdown [ e' d' cis'] \stemup f'8 [g'16^\prall f'32 g']
[a'16 e' f' d'] \stemdown [e' cis' d' b] |
% 13
[cis'16 a b aes] \stemup cis'4 a,4^\prall^"\\textnatural"
r^\fermata | \break
% 14
[a'16 b' cis'' d''] [e'' cis'' f'' d''] e''4. a'8 |
% 15
[b'16 g' cis'' a'] [d'' b' e'' cis''] [fis'' d'' g'' e''] [a''
c'' bes' a'] |
% 16
[bes'16 g' c'' a'] [d'' bes' a' g'] [fis' d' g' e'] [a' c' bes a]
|
% 17
\stemdown g16 s a s bes s c' s d' s e' s f' s g' s |
% 18
[a'16 f' d' bes] [g' e' c' e'] \stemup c''4 bes'4^\prall |
% 19
[c''8 f''] [bes' e''] [a'16 f' c'' a'] [f'' c'' a'' f''] |
% 20
[e''16 g'' c'' e''] g'8  \stemdown fis' g'16 s g8 [g g,] |
% 21
[d8 d'] d'16 \stemup [d'' c'' b'] [c'' a' e'' c''] [a'' e'' c'''
a''] |
% 22
[gis''16 b'' e'' aes''] [b' d'' cis'' b'] \stemdown [cis''8 a']
[a' a'] |
% 23
f,8 a' [f' f'16 a'] \stemup [bes'8 g''] g''4^\prall~ |
% 24
[g''16 f'' e'' d''] [c'' bes' a'] s \stemdown f,8 f' [f' f'] |
% 25
d,8 d' [f' d'16 f'] \stemup [g'8 e''] e''4^\prall~ |
% 26
[e''16 d'' cis'' b'] [a' g' f' e'] [f' d' g' e'] [a' f' bes' g']
|
% 27
\stemdown [f'16 d' g' e'] [a' f' bes' g'] \stemup f' s g' s a' s
bes' s |
% 28
[a'8 d''] [g' cis''] \stemdown [f'16 d' bes g] [e' cis' a cis'] |
% 29
\stemup [a'8 d''] [g' e''] [f'16 d' g' e'] [a' f' b' g'] |
% 30
[cis''16 a' d'' e''] [f'' d'' e'' cis''] [d'' a' bes' g']
\stemdown  [a' f' g' e'] |
% 31
[f'16 d' e' cis'] d'4 \stemup d4^\prall r4^\fermata |
} {
s1 s
% 3
s2 s8 g' bes' g' |
% 4
g'8 g' g' s s2 |
% 5
s2 s8 e' g' g' |
% 6
a8 e' e' s s2 |
s1 s s s s s s s s s s s
% 19
s8 a'' s g'' s2 |
s1 s s
% 23
s8 f' a' s s2 |
% 24
s2 s8 a' a' a' |
% 25
s8 f' d' s s2 |
% 26
\stemup s2 d''4 s |
% 27
\stemdown d'16 s e' s f' s g' s s2 |
% 28
s8 f'' s e'' s2 |
% 29
s8 f'' s cis''
} >
\context Voice = vTwo <
{s1 s s s s
%6
\stemdown  s2 d'16 s s4. |
s1 s s s s s s s s s s s s s s s s s s
%26
\stemup s2 d''4 s |
%27
s2 d''4 s2
s1
%29
s2 d''8}
>
\context Voice = vThree <
{
% 1
\stemdown s2 [d'16 e' f' g'] [a' a cis' a] |
% 2
[d'8 e'] [f' g'] [a' a'] [a' a'] |
% 3
[a'8 a'] [a' f'] g \stemup g'' g''4^\prall~
% 4
[g''16 f'' e'' d''] [c'' bes' a' g'] a'8 \stemdown  a' [f' a'] |
% 5
[d'8 f'] [d' d'] e \stemup e'' e''4^\prall~
% 6
[e''16 d'' cis'' b'] [a' g' f' e'] [f' d' g' e'] [a' f' e' d'] |
% 7
[cis'16 a d' b] [e' cis' f' d'] [g' e' a' f'] [b' g' cis'' a'] |
% 8
[d''16 b' e'' cis''] [f'' d'' g'' e''] a''4 g''4^\prallmordent |
% 9
\stemdown [f''16 d'' bes' g'] [e'' cis'' a' cis''] \stemup a''4
g''4^\prallmordent |
% 10
\stemdown [d'8 e'] [f' g'] [a b] [cis' a] |
% 11
[d8 e] [f g] a16 s b s cis' s d' s |
% 12
e16 \stemup [g' f' e'] \stemdown [d'8 bes] a,4 s8 \stemup f' |
% 13
[e'8 d'] \stemdown [a16 e cis e] s2 |
% 14
s2 [a16 b cis' d'] [e' cis' f' d'] |
% 15
g'16 s a' s b' s cis'' s [d'8 e'] [fis' d'] |
% 16
[g8 a8] [bes c'] [d e] [fis d] |
% 17
\stemup [bes16 g c' a] [d' bes e' c'] [f' d' g' e'] [a' f' bes'
g'] |
% 18
c''4 bes'4^\prall \stemdown [a'16 f' d' bes] [g' e' c' e'] |
% 19
[a'16 f' d' bes] [g' e' c' e'] f' s f8 [f f,] |
% 20
[c8 c'] c'16 \stemup [c'' bes' a'] [bes' g' d'' bes'] [g'' d''
bes'' g''] |
% 21
[f''16 a'' d'' f''] a'8 \stemdown gis' a'16 s a8 [a a,] |
% 22
[e8 e,] [e, e,] a, \stemup a'' a''4^\prall^"\\textflat"~ |
% 23
[a''16 g'' f'' e''] [d'' c'' bes'] s \stemdown g,8 bes' [bes'
bes'] |
% 24
e,8 e' [g' e'16 g'] \stemup [a'8 f''] f''4^\prall~ |
% 25
[f''16 e'' d'' c''] [bes' a' g'] s \stemdown e,8 g' [g' g'] |
% 26
a,8 a [e' a] d'16 s e' s f' s g' s |
% 27
s2 [d'16 d' e' e'] [f' f' g' g'] |
% 28
[f'16 d' bes g] [e' cis' a cis'] \stemup [a'8 d''] [g' e''] |
% 29
\stemdown [f'16 d' bes g] [e' cis' a cis'] [d8 e] [f g] |
% 30
[a8 g] [a a,] d4 s8 \stemup bes' |
% 31
[a'8 g'] f'16 \stemdown [a f a] s2 |
} {
s1 s
% 3
f'8 f' f' s s2 |
% 4
s2 s8 f' a' f' |
% 5
f'8 d' f' s s2 |
s1 s s s s s s s s s s s s s s s s
% 23
s2 s8 g' g' g' |
ii% 24
s8 g' e' s s2 |
% 25
s2 s8 e' e' e' |
% 26
s8 e' a s s2 |
s1
% 28
s2 s8 f'' s cis'' |
} >
\context Voice = vFour <
{s1 s s s s s s s s
%10
\stemdown d''16 s s4. a'16 s s4. |
%11
d'16 s s4. s2 |
s1 s s
%15
s2 d''16 s s4. |
%16
g'16 s s4. d'16 s s4. |
s1 s s s s s s s s s
%27
\stemup d''4 s2. |
s1
%29
\stemdown s2 d'16}
>
>}
\paper{barsize=50.0; \translator {\StaffContext \consists
Bar_number_engraver;}}
}
\version "1.1.66";
