	%{
Here are the first four sonatas with barring, stem directions
etc., laid out so that two note shapes can be used when that is
ready. (Freenet will chop some of the code lines - I've got it
as one Mudela line per bar.) As you will quickly see, K.4 raises
some notation and collision problems. I've used the modern
tied-notes syntax you prefer, instead of the way Scarlatti wrote
it (bar1.jpeg). Scarlatti had a point, didn't he! (150 years
later, Longo agreed - bar1 is from his edition.)

Harpsichord music is written much closer than organ (or piano)
music because harpsichords rely on the transfer of energy from
one string directly to other harmonically-related strings to
build power and maintain sound. That only happens when the
strings are close to each other on the soundboard. In fact, that
is one of the ways of judging what instrument a keyboard piece
was written for - organ pipes don't talk to each other, organ
sound is always fighting for clarity, and close harmonies sound
thick, so good organ music is much more widely spaced. K.4 is
typical of mainstream harpsichord music.

I'll work on the musical aspects of the notation - Lily can be
told to do it one way as easily as the other. (I really
appreciate that.) But, would you give some thought to the best
way of dealing with 'multi-voice' collisions? In the long term,
particularly if you go ahead with automating beaming, you will
need a general anti-collision system. But, for the next 6 months
or so, to keep the Scarlatti project going, might it be faster
to provide for setting the stem length of individual notes? Or
some other similar workaround?

Oh yes - when the middle note of a chord is trilled, Scarlatti
wrote the praller directly in front of the note, whether the
note was on a line or a space. And, he wrote most of the K.3
fermatae on top of a bar line to indicate that a sound gap was
wanted, not that the prior note be extended in length.

John

----------------------------------------------------
%}

----------------------------------------------------

\score {
 \header {title="Domenico Scarlatti - Sonata K.3 L.378";
          description="Presto";}
 % vOne is right hand outside part, vTwo r.h. inside part,
 % vThree left hand outside, vFour l.h. inside.
 \notes {
   \type Staff=staffOne <
    \property Staff.nolines=11
    \property Voice.pletvisibility=0
    \time 2/2;
    \clef alto;
    \type Voice=vOne <{
% 1
\stemup s1
% 2
s4 [e'16 d' c' b] a4 s |
% 3
s4 a' e' c'' |
% 4
b'4 a'2 gis'4 |
% 5
s4 c'' a' e'' |
% 6
d''4 c''2 b'4 |
% 7
s4 c'' g' f'' |
% 8
e''4 d''2 cis''4 |
% 9
s4 d'' a' g'' |
% 10
fis''4 e''2 dis''4 |
% 11
[e''8 d''] c''2 b'4~b'
% 12
a'2 g'4~g'
% 13
fis'2 e'4~e'
% 14
d'2 c'4~c'
% 15
b2 a4 |
% 16
s4 c' dis' fis' |
% 17
a'4 c'' dis'' fis'' |
% 18
a''4 c'''2 [b''8 a''] |
% 19
g''4 f'' e'' d''^\fermata |
s1 s
% 22
\stemdown s4 a, b, cis |
% 23
d4 e f2 |
% 24
\stemup f4 aes b d' |
% 25
f'4 aes' b' d'' |
% 26
f''4 aes''2 [g''8 f''] |
% 27
ees''4 d'' c'' d'' |
% 28
g'4 b' c'' \stemdown f |
% 29
e4 e'2 \stemup d''4 |
% 30
e'4 b' c'' \stemdown b, |
% 31
a,4 a \stemup c'' d'' |
% 32
a4 b c' d' |
% 33
e'4 f' g' a' |
% 34
b'4 [a'8 g'] g''4 c'' |
% 35
c''4~[c''16 b' c'' d''] d''4.^\prall c''8 |
% 36
c''4 c''' b'' bes''
% 37
a'' aes''2 g''4~g''
% 38
f''2 dis''4~dis''
% 39
d''2 c''4~c''
% 40
b'4 c'' d'' |
% 41
e'4 f' g' a' |
% 42
b'4 [a'8 g'] g''4 c'' |
% 43
c''4~[c''16 b' c'' d''] d''4.^\prall c''8 |
% 44
c''4 [g''16 f'' e'' d''] c''4 s |
% 45
s4 [g16 f e d] c4 s |
% 46
s4 c'' g' a' |
% 47
e''4 c'' d'' b' |
% 48
e'2. s4 |
% 49
s4 [g'16 f' e' d'] c'4 s |
% 50
s4 c'' g' ees''~ees''
% 51
d''2 c''4 |
% 52
bes'4 s2 [d'16 c' bes a] |
% 53
g4 s2 g''4 |
% 54
g''4 d'' f'' e'' |
% 55
d''4 [a''16 g'' f'' e''] d''4 s |
% 56
s4 [a16 g f e] d4 s |
% 57
s4 d'' a' f'' |
% 58
f'' e''4 bes'2 |
% 59
cis''4 s2. |
% 60
\stemdown s4 a' gis' g' |
% 61
fis'4 f' e'2 |
% 62
dis'4 d' cis' c' |
% 63
b4 bes a2 |
% 64
gis4 g fis f |
% 65
\stemup s4 e gis b |
% 66
d'4 f' gis' b' |
% 67
d''4 f''2 e''8 d'' |
% 68
c''4 b' a' g'^\fermata |
s1 s
% 71
\stemdown s4 d e fis |
% 72
g4 a bes2 |
% 73
\stemup e'4 g' bes' e'' |
% 74
bes''2. [a''8 gis''] |
% 75
a''4 g''2 [f''8 e''] |
% 76
f''4 e''2 [d''8 cis''] |
% 77
d''4 c''2 [b'8 a'] |
% 78
gis'4 gis'' a'' \stemdown d |
% 79
c4 c'2 \stemup b''4 |
% 80
c''4 gis'' a'' \stemdown g, |
% 81
f,4 f2 \stemup b''4 |
% 82
f'4 gis' a' b' |
% 83
c''4 d'' e'' f'' |
% 84
gis'4 [a'8 b'] e'4 d'' |
% 85
c''4 d'' b'2 |
% 86
a'4 a'' gis'' g'' |
% 87
fis''4 f''2 e''4~e''
% 88
d''2 c''4~c''
% 89
b'2 a'4~a'
% 90
gis'4 a' d'' |
% 91
a'2 gis' |
% 92
a'4 s2 [e'16 d' c' b] |
% 93
a4 s2 d''4 |
% 94
c''2 gis' |
% 95
a'2. s4 |
} {
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s s s s
s s s s s s s s s s s s s
% 46
s2. f''4 |
% 47
g'2 f' |
% 48
c''2. s4 |
s1 s
% 51
g'2 fis' |
s1
% 53
s2. bes'4 |
% 54
a'4 f'' d'' cis'' |
% 55
f''4 s2. |
s1 s
% 58
bes'2 e''4 d'' |
% 59
a'4 s2. |
s1 s s s s s s s s s s s s s s s s s s s s s s s s
% 85
a'2 gis' |
s1 s s s s
% 91
e'2 b' |
s1
% 93
s2. a'4 |
% 94
a'2 b' 
} {
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s s
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s s
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s s
% 91
c''4 d''4 s2 |
s1
% 93
s2. f'4 |
% 94
e'2 e'2
     }>
    \type Voice=vTwo <
     {
     %- right hand unchorded notes -
     }>
    \type Voice=vThree <{
% 1
\stemdown s2.  [e''16 d'' c'' b'] |
% 2
a'4 s2 [e16 d c b,] |
% 3
a,4 s2 a4 |
% 4
e4 c' b2 |
% 5
a4 s2 c'4 |
% 6
g4 e' d'2 |
% 7
c'4 s2 d'4 |
% 8
a4 f' e'2 |
% 9
d'4 s2 e'4 |
% 10
b4 g' fis'2 |
% 11
e'2 dis'4 d' |
% 12
cis'4 c' b2 |
% 13
bes4 a gis g |
% 14
fis4 f e2 |
% 15
dis4 d cis c |
% 16
b,2  s |
s1
% 18
s4 b, cis dis |
% 19
e4 f g2 |
% 20
\stemup g4 bes cis' e' |
% 21
g'4 bes' cis'' e'' |
% 22
g''4 bes''2 [a''8 g''] |
% 23
f''4 e'' d'' c''^\fermata |
s1 s
% 26
\stemdown s4 g, a, b, |
% 27
c4 d dis f |
% 28
g4 g'2 \stemup d''4 |
% 29
g'4 b' c'' \stemdown d |
% 30
c4 c'2 \stemup d''4 |
% 31
c'4 b'2 \stemdown g,4 |
% 32
f,4 f e d |
% 33
c4 d e f |
% 34
g,4 g e f |
% 35
g4 f g g, |
% 36
c2 s
% 37
s4 c'' b' bes' |
% 38
a'4 gis' g'2 |
% 39
fis'4 f' e' dis' |
% 40
d4 d' c' b |
% 41
c4 d e f |
% 42
g,4 g e f |
% 43
g4 f g g, |
% 44
c2 s4 [g'16 f' e' d'] |
% 45
c'4 s2 [g,16 f, e, d,] |
% 46
c,4 c s f |
% 47
g2 g, |
% 48
c2.  [g''16 f'' e'' d''] |
% 49
c''4 s2 [g16 f e d] |
% 50
c4 s2 c'4 |
% 51
d'2 d |
% 52
g4 [d''16 c'' bes' a'] g'4 s2
% 53
[d16 c bes, a,] g,4 g |
% 54
a2 a, |
% 55
d2 s4 [a'16 g' f' e'] |
% 56
d'4 s2 [a,16 g, f, e,] |
% 57
d,4 s2 d'4 |
% 58
g2 g, |
% 59
a,4 \stemup a'' gis'' g'' |
% 60
fis''4 f''2 e''4~e''
% 61
d''2 c''4~c''
% 62
b'2 a'4~a'
% 63
g'2 f'4~f'
% 64
e'2 d'4 |
% 65
\stemdown e,2^\fermata s |
s1
% 67
s4 e fis gis |
% 68
a4 b c'2 |
% 69
\stemup fis4 a c' ees' |
% 70
fis'4 a' c'' ees'' |
% 71
c'''2.  [bes''8 a''] |
% 72
bes''4 a'' g'' f'' |
s1
% 74
\stemdown s4 c d e |
% 75
f2 e |
% 76
d2 c |
% 77
b,2 a, |
% 78
e4 e'2 \stemup b''4 |
% 79
e''4 gis'' a'' \stemdown b, |
% 80
a,4 a2 \stemup b''4 |
% 81
a'4 gis'' a'' \stemdown e, |
% 82
d,4 d' c' b |
% 83
a4 b c' d' |
% 84
e4 d' c' d' |
% 85
e'4 d' e' e |
% 86
a2 s |
% 87
s4 a' gis' g' |
% 88
fis'4 f' e'2 |
% 89
dis'4 d' cis' c' |
% 90
d'2 c'4 d |
% 91
e4 d e e, |
% 92
a,4 [e''16 d'' c'' b'] a'4 s2
% 93
[e16 d c b,] a,4 d |
% 94
e2 e, |
% 95
a,2.
} {
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s s
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s s
s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s s
% 90
b2 a4 
     } >
    \type Voice=vFour <
     {
     %- left hand unchorded notes -
     }
  >>}
 \paper{barsize=50.0;  \translator {\StaffContext \consists
Bar_number_engraver;}}
}

----------------------------------------------------

\score {
 \header {title="Domenico Scarlatti - Sonata K.4 L.390";
          description="Allegro";}
 % vOne is right hand outside part, vTwo r.h. inside part,
 % vThree left hand outside, vFour l.h. inside.
 \notes {
   \type Staff=staffOne <
   \property Staff.nolines=11
    \keysignature bes;
    \time 4/4;
    \clef alto;
    \type Voice=vOne <
{
% 1
\stemup \skip 4..*15/7; g'16 |
% 2
[g'16 bes' a' c''] [bes' d'' c'' ees''] d''4. g''8 |
% 3
[fis''8 g''] [a'' c''] [bes' c''] [d'' g'] |
% 4
[fis'8 g'] [a' c'] [bes c'] [d' g'] |
% 5
[fis'8 g'] a' \stemdown d [g bes,] [c d] |
% 6
[g,8 g8.] s16 \stemup ees'8 [f' a'] [bes'16 ees'' d'' c''] |
% 7
[d''8 ees''] [f'' bes'] \stemdown [f' d'] [ees'16 g' b d'] |
% 8
[c'16 ees' d' f'] [ees' g' f' aes'] \stemup  [b' d'' cis'' e'']
[d''8 e''] |
% 9
[f''8 g''] [a'' d''] \stemdown [a16 cis' b d'] [cis' e' a cis'] |
% 10
\stemup [f'8 g'] [a' d'] [cis' d'] [e'16 g' f' e'] |
% 11
[f'16 a' d''8]~[d''16 e'' c''8]~[c''16 d'' bes' g'] [a' e'' f''8]
|
% 12
e''8 d''4~[d''16 cis''] [d'' bes' a' g'] [a' bes' cis'' a'] |
% 13
[bes'16 cis'' d'' e''] [a' g'' f'' e''] [f'' e'' d'' c''] [d''
e'' f'' g''] |
% 14
[a''16 cis'' d'' e''] a' \stemdown [f' e' d'] a,4~a,16 \stemup
[d'' cis'' b'] |
% 15
[a''16 cis'' d'' e''] [a' d'' cis'' b'] cis''4~[cis''16 cis'' d''
e''] |
% 16
[a'8 a''8]~[a''16 g'' f'' e''] [f''8 e''16 d''] [cis''8 d''] |
% 17
cis''4 \stemdown [cis'16 a g f] [e8 a] d \stemup g' |
% 18
[a'16 cis' d' e'] a \stemdown [f e d] a,4~a,16 \stemup [d' cis'
b] |
% 19
cis'4~[cis'16 cis' d' e'] [a8 a']~[a'16 g' f' e'] |
% 20
[f'16 a' d''8]~[d''16 e'' cis''8] d''4 [a'8 g'] |
% 21
fis'8 d'4 cis'8 [d'8. e'16] [fis'8. g'16]~
% 22
[g'16 a'8 fis'16] [bes'8. cis''16] [cis''8 d''] r8. d'16 |
% 23
[d'16 fis' e' g'] [fis' a' g' bes'] a'4. bes'8 |
% 24
[c''8 d''] [ees'' fis'] [g' a'] [bes' b'] |
% 25
[c''8 d''] [ees'' g''] [f'' ees''] [d'' c''] |
% 26
[g''16 b' c'' d''] g' \stemdown [ees' d' c'] g,4~g,16 \stemup
[c'' b' a'] |
% 27
[g''16 b' c'' d''] [g' c'' b' a'] b'4~b'16 \stemdown [f' ees' d']
|
% 28
g8 \stemup g''8~[g''16 f'' ees'' d''] ees''8 \stemdown f [g aes]
|
% 29
g,8 \stemup [ees'16 f'] [d'8 ees'] [f' g'] g'16 \stemdown [c' bes
a] |
% 30
[g16 g'] \stemup cis''8 [d''16 c''' bes'' a''] [bes''8 a''16 g'']
[fis''8 g''] |
% 31
[a''16 fis'' g'' a''] d'' \stemdown [bes' a' g'] d,4~d,16 \stemup
[g'' fis'' e''] |
% 32
[fis''16 fis'' g'' a''] d'' \stemdown [bes' a' g'] [d'' a' fis'
a'] s \stemup  [fis''16 g'' a''] |
% 33
d''8 g''8~[g''16 c''' bes'' a''] [bes''8 a''16 g''] [fis''8 g'']
|
% 34
[a''16 a' bes' c''] \stemdown [fis' d' c' bes] [a8 d] g s |
% 35
d4~d16 \stemup [g' fis' e'] [d'' fis' g' a'] d' \stemdown [bes a
g] |
% 36
d,4~d,16 \stemup [g' fis' e'] d'' [fis' g' a'] [d' g'] [fis' e']
|
% 37
d''4~d''16 [fis' g' a'] d' \stemdown [bes a g] [d a g fis] |
% 38
[g8 ees] [d d,] g,4 \stemup [bes'16 g' a' fis'] |
% 39
bes'8  g'4 fis'8 [g'8. a'16] [bes'8 c''] |
% 40
[d''8. d''16] [ees''8 fis'' ] [fis'' g''] r8. s16 |
}
{
s1 s s s s s s s s s s
%12
cis''8 s s4 s2 |
s1 s s s
% 17
<e''4 a'> s4 s2 |
s1 s s s
%22
s4 s8. a'16 d'4 s4 |
s1
%24
s2 s4. d'8 |
%25
ees'8 f' g' s s2 |
s1 s s
%29
s4. c'8 s4 ees'16
}
    >
    \type Voice=vTwo <
 {
s1 s s s s s s s s s
% 11
\stemdown s4. [c''16 g'] fis'8 s8 [a'8. b'16] |
% 12
s16 \skip 4..*71/7;
% 16
[f''16 a' bes'8]~[bes'16 a'8 g'16] |
s1 s s
% 20
[f'8. f'16] [e'8. g'16]~[g' bes' a' g'] [fis' d' e' cis'] |
% 21
[d'16 c' bes a] [g bes a g] [fis a g8] [a16 d bes8] |
% 22
[ces'8 d']~[d'16 g' e' g'] s2 |
s1 s
% 25
s4. \skip 4..*64/7;
% 29
c'8 [f'16 d' ees' d']  \skip 4..*12/7;
% 30
[bes''16 d'' ees''8]~[ees''16 d''8 c''16] |
% 31
\skip 4..*40/7;
% 33
[bes''16 d'' ees''8]~[ees''16 d''8 c''16] |
% 34
\skip 4..*48/7; |
% 37
fis'4~fis'16 \skip 4..*27/7; |
% 39
[g'16 f' ees' d'] [c' ees' d' c']~[c' bes c'8]~[c'16 d'8 fis'16]~
% 40
[fis'16 a' bes' g']~[g' c'' a'd'] <g'4 bes'>
}   >
    \type Voice=vThree <
{
% 1
\stemdown \skip 4..*23/7;
% 2
g16 [g bes a c'] [bes d' c' ees'] |
% 3
[d'16 fis' e' g'] [fis' a' d' fis'] [g bes a c'] [bes d' c' ees']
|
% 4
[d16 fis e g] [fis a d fis] [g, bes, a, c] [bes, d c ees] |
% 5
[d16 fis e g] fis \stemup [c' bes a] [bes d' g'8]~ [g'16 a'
fis'8] |
% 6
[g'16 ees' d' c'] \stemdown [d' bes a g] [a ees' d' c'] [d'8 a] |
% 7
[bes16 d' c' ees'] [d' f' ees' g'] \stemup [a' c'' b' d''] [c''8
d''] |
% 8
[ees''8 f''] [g'' c''] \stemdown [g' e'] [f'16 a' cis' e'] |
% 9
[d'16 f' e' g'] [f' a' g' bes'] \stemup [cis''8 d''] [e'' g'] |
% 10
\stemdown [d16 f e g] [f a g bes] [a, cis b, d] [cis8 a,] |
% 11
[d8 f] [g a] [d g] [cis d] |
% 12
a2 d' |
% 13
[g'8 e'] [f' cis'] d' a'4 g'8 |
% 14
a,4~a,16 \stemup [d'' cis'' b'] [a'' cis'' d'' e''] a' \stemdown
[f' e' d'] |
% 15
a,4~a,16 [f' e' d'] [a' e' cis' e'] [a g' f' e'] |
% 16
[a16 f' e' d'] [a e' d' cis'] [d'8 g] [a bes] |
% 17
a,16 \stemup [e' f' g'] [e'8 f'] [g' f'16 e'] f' \stemdown [d'
cis' b] |
% 18
a,4~a,16 \stemup [d' cis' b] [a' cis' d' e'] a \stemdown [f e d]
|
% 19
[a16 e cis e] [a, g f e] [a, f e d] [a, e d cis] |
% 20
[d8 g] [a a,] d,2 |
% 21
d,2 d, |
% 22
d,2 d,4 \skip 4..*11/7;
% 23
d16 [d fis e g] [fis a g bes] |
% 24
[a16 c' bes d'] [c' ees' d' c'] [bes c' a bes] [g a f g] |
% 25
[ees16 f d ees] [c d bes, c] [aes, bes, g, aes,] [f, aes, g, f,]
|
% 26
g,4~g,16 \stemup [c'' b' a'] [g'' b' c'' d''] g' \stemdown [ees'
d' c'] |
% 27
g,4~g,16 [ees' d' c'] [g' d' b d'] g \stemup [b' c'' d''] |
% 28
[g'16 ees'] \stemdown [d' c'] [g d' c' b] [c' g'] \stemup [d''
c''] [b'8 c''] |
% 29
[d''16 d'] \stemdown g8 [b16 g f ees] [d8 g] c \stemup a' |
% 30
bes'8 \stemdown [fis'16 e'] [fis'8 d'] [g' c'] [d' ees'] |
% 31
d,4~d,16 \stemup [g'' fis'' e''] [fis'' fis'' g'' a''] d''
\stemdown [bes' a' g'] |
% 32
d4~d16 \stemup [g'' fis'' e''] fis''4 \stemdown [d'16 c'' bes'
a'] |
% 33
[d'16 bes' a' g'] [d' a' g' fis'] [g'8 c'] [d' ees'] |
% 34
[d8 d'] \stemup [a' bes'] [c'' bes'16 a'] [bes'8 c''] |
% 35
[d''16 fis' g' a'] d' \stemdown [bes a g] d,4~d,16 \stemup [g'
fis' e'] |
% 36
[d''16 fis' g' a'] d' \stemdown [bes a g] d,4~d,16 [bes a g] |
% 37
[d'16 a fis a] [d c' bes a] d8 \stemup d''8. [c''16 bes' a'] |
% 38
[bes'16 d'' g''8]~[g''16 a''8 fis''16] g''4 s4 |
% 39
\stemdown g,2 g, |
% 40
g,2 g,4
} 
{s1 s s s s s s s s s s s s s s s
%17
s4 s8 d' s2 |
s1 s s s s s s s s s s
% 29
g'16  s8. s2 s8 fis' |
%30
g8 s s2. |
s1 s s
%34
s4. g'8}
{s1 s s s s s s s s s s s s s s s s s s s s s s s s s s s
% 29
b'16}
    >
    \type Voice=vFour <
     {
\stemdown \skip 4..*74/7;
% 5
[g'16 bes a8.] c'16~c'
% 6
\skip 4..*81/7;
% 11
\stemup [f16 f' e'8] \skip 4..*11/7;
% 12
\stemdown [e'16 f' d'] [e' f' g'8] s [fis'16 e'] [fis' g' a'
fis'] |
% 13
s2 s8 a'4 g'8 |
% 14
\skip 4..*48/7; |
% 17
s2 [g'16 e' d' cis'] \skip 4..*39/7;
% 20
\skip 4..*135/7;
% 28
aes'8 [b16 g'8 f'16] |
% 29
s16 \skip 4..*13/7; s8 |
% 30
s8 \skip 4..*68/7;
% 34
s8 [c''16 g'8 fis'16] [bes'' g' f' ees'] |
% 35
\skip 4..*50/7;
% 38
[g''16 c''] [bes'8 a'] s16 [ees'' d'' c'']
}  >>}
 \paper{barsize=50.0; \translator {\StaffContext \consists
Bar_number_engraver;}}
}

----------------------------------------------------

