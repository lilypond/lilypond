

\score {
 \header {title="Domenico Scarlatti - Sonata K.2 L.388";
          description="Presto";}
 % vOne is right hand outside part, vTwo r.h. inside part,
 % vThree left hand outside, vFour l.h. inside.
 \notes {
   \context Staff=staffOne <
    \property Staff.nolines=11
    \property Voice.pletvisibility=0
    \keysignature fis;
    \time 3/8;
    \clef alto;
    \context Voice=vOne <{
%1
\stemup [g''8 g' g'] |
% 2
g'4.^\prall |
% 3
[g'16 d' b' g' d'' b'] |
% 4
g''4. |
% 5
[b'8 c'' d''] |
% 6
c''4 b'8 |
% 7
[c''16 a'' b' g'' a' fis''] |
% 8
[b'16 g'' a' fis'' g''8] |
% 9
\stemdown [g'8 a' b'] |
% 10
[a'8 fis' g'] |
% 11
[c'8 d' d] |
% 12
[g8 d g,] |
% 13
[b'16 d'' a' d'' g' cis''] |
% 14
[fis'16 d'' e'' cis'' d'' a'] |
% 15
\stemup [g''8 fis'' e''] |
% 16
a''8 s4 |
% 17
[d''16 a' cis'' a' b' gis'] |
% 18
[e''16 a' b' gis' a' e'] |
% 19
\stemdown [fis'8 e' d'] |
% 20
cis'8 s4 |
% 21
\stemup [d''16 g' a'' fis' cis'' e'] |
% 22
[d''16 a' fis'] s8. |
% 23
\stemdown [b8 a g] |
% 24
fis8 s16 \stemup [d' fis' a'] |
% 25
[d''16 g' a'' f' cis'' e'] |
% 26
[d''16 a' f'] s8. |
% 27
\stemdown [bes8 a g] |
% 28
f8 s16 \stemup [d' f' a'] |
% 29
[d''16 g' cis'' fis' b' e'] |
% 30
[a'16 d' g' cis' fis' d'] |
% 31
[g'16 e' fis' d' e' cis'] |
% 32
[d'8 cis' b] |
% 33
[a8 g' fis'] |
% 34
[e'8 d'8. cis'16] |
% 35
[d'16 a fis' d' a' fis'] |
% 36
[d''16 a' fis'' d'' a'' a'] |
% 37
d''4. |  \break
% 38
[d''8 d' d'] |
% 39
d'4.^\prall |
% 40
[d'16 a fis' d' a' fis'] |
% 41
d''4.^\prall |
% 42
[fis'8 g' a'] |
% 43
g'4^\prall fis'8 |
% 44
[g'16 e'' fis' d'' e' cis''] |
% 45
[fis'16 d'' e' cis'' d''8] |
% 46
\stemdown [d'8 e' fis'] |
% 47
[e'8 cis' d'] |
% 48
[g8 a a,] |
% 49
[d8 a, d,] |
% 50
\stemup [d''16 a' c'' a' b' gis'] |
% 51
[e''16 a' b' gis' a' e'] |
% 52
\stemdown [f'8 e' d'] |
% 53
c'8 s4 |
% 54
[e'8 d' c'] |
% 55
b8 s4 |
% 56
\stemup [c''16 g' b' g' a' fis'] |
% 57
[d''16 g' a' fis' g' d'] |
% 58
\stemdown [e'16 c'' d' b' c' a'] |
% 59
[b16 g' a fis' g g'] |
% 60
[c'8 d' e'] |
% 61
d'8 s4 |
% 62
[e8 d c] |
% 63
b,8 s \stemup [b16 d'] |
% 64
[g'16 c' d'' b fis' a] |
% 65
[g'16 d' b g] s8 |
% 66
\stemdown [ees8 d c] |
% 67
bes,8 s \stemup [ais16 d'] |
% 68
[g'16 c' d'' bes fis' a] |
% 69
[g'16 d' ais g] s8 |
% 70
[g''16 c'' fis'' b' e'' a'] |
% 71
[d''16 g' c'' fis' b' g'] |
% 72
[c''16 a' b' g' a' fis'] |
% 73
[g'8 fis' e'] |
% 74
[d'8 c' b] |
% 75
[a16 c' b g' a fis'] |
% 76
[g'16 d' b g b d'] |
% 77
[g'16 d' b' g' d'' d'] |
% 78
g'4. |}
{s1 s s s s
%6
d4
     }>
    \context Voice=vThree <{
% 1
\stemdown s4. |
% 2
[g8 g, g,] |
% 3
g,4. |
% 4
[g16 d b g d' b] |
% 5
[g'8 a' b'] |
% 6
[a'8 fis' g'] |
% 7
[c'8 d' fis'] |
% 8
[g'8 d' g] |
% 9
\stemup [b'8 c'' d''] |
% 10
c''4^\prall b'8 |
% 11
[c''16 a'' b' g'' a' fis''] |
% 12
[b'16 g'' a' fis'' g''8] |
% 13
[g''8 fis'' e''] |
% 14
a''8 s4 |
% 15
\stemdown [b'16 d'' a' d'' g' cis''] |
% 16
[fis'16 d'' e'' cis'' d'' a'] |
% 17
[fis'8 e' d'] |
% 18
cis'8 s4 |
% 19
\stemup [d''16 a' cis'' a' b' gis'] |
% 20
[e''16 a' b' gis' a' e'] |
% 21
\stemdown [b8 a g] |
% 22
fis8 s16 \stemup [d' fis' a'] |
% 23
[d''16 g' a'' fis' cis'' e'] |
% 24
[d''16 a' fis'] s8. |
% 25
\stemdown [bes8 a g] |
% 26
f8 s16 \stemup [d' f' a'] |
% 27
[d''16 g' a'' f' cis'' e'] |
% 28
[d''16 a' f'] s8. |
% 29
\stemdown [b8 a g] |
% 30
[fis8 e d] |
% 31
[g8 a a,] |
% 32
[b,16 g a, fis g, e] |
% 33
[fis,16 d e, cis d, d] |
% 34
[g,16 e a, fis e8] |
% 35
d,4. |
% 36
d,4. |
% 37
d,4. |
% 38
s4. |
% 39
[d8 d, d,] |
% 40
d,4. |
% 41
[d16 a, fis d a fis] |
% 42
[d'8 e' fis'] |
% 43
[e'8 cis' d'] |
% 44
[g8 a cis'] |
% 45
[d'8 a d] |
% 46
\stemup [fis'8 g' a'] |
% 47
g'4^\prall fis'8 |
% 48
[g'16 e'' fis' d'' e' cis''] |
% 49
[fis'16 d'' e' cis'' d''8] |
% 50
\stemdown [f'8 e' d'] |
% 51
c'8 s4 |
% 52
\stemup [d''16 a' c'' a' b' gis'] |
% 53
[e''16 a' b' gis' a' e'] |
% 54
[c''16 g' b' g' a' fis'] |
% 55
[d''16 g' a' fis' g' d'] |
% 56
\stemdown [e'8 d' c'] |
% 57
b8 s4 |
% 58
\stemup [g''8 fis'' e''] |
% 59
[d''8 c'' b'] |
% 60
[a'16 b' c'' b' a' g'] |
% 61
[a'16 fis' g' e' fis' d'] |
% 62
[g'16 c' d'' b fis' a] |
% 63
[g'16 d' b g] s8 |
% 64
\stemdown [e8 d c] |
% 65
b,8 s \stemup [b16 d'] |
% 66
[g'16 c' d'' bes fis' a] |
% 67
[g'16 d' bes g] s8 |
% 68
\stemdown [ees8 d c] |
% 69
bes,8 s [bes16 d'] |
% 70
[e'8 d' c'] |
% 71
[b8 a g] |
% 72
[c'8 d' d] |
% 73
[e16 c' d b c a] |
% 74
[b,16 g a, fis g, g] |
% 75
[c8 d d,] |
% 76
g,4. |
% 77
g,4. |
% 78
g,4. |
     } >
 >}
 \paper{barsize=50.0;  \translator {\StaffContext \consists
Bar_number_engraver;}}
}

\version "1.1.52";
