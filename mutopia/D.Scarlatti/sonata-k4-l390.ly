
\score {
 \header {title="Domenico Scarlatti - Sonata K.4 L.390";
          description="Allegro";}
 % vOne is right hand outside part, vTwo r.h. inside part,
 % vThree left hand outside, vFour l.h. inside.
 \notes {
   \context Staff=staffOne <
   \property Staff.nolines=11
    \keysignature bes;
    \time 4/4;
    \clef alto;
    \context Voice=vOne <
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
    \context Voice=vTwo <
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
    \context Voice=vThree <
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
    \context Voice=vFour <
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
\version "1.0.16";
