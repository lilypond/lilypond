%K1
rh = {\property Thread.noteHeadStyle = ""}
lh = {\property Thread.noteHeadStyle = "diamond"}
\score {
\notes {
\context Staff = staffOne <
\property Staff.numberOfStaffLines = "11"
%\property Staff.timeSignatureStyle = "C"
\keysignature bes;
\time 4/4;
\clef alto;
\context ThreadedVoice = vOne <
\context Thread=tOne{
\stemup \slurup
%1
\rh [d''16 e'' f'' g''][a'' a' cis'' a'] d''4. e''8|
%2
[f''16 d'' g'' e''][a'' f'' e'' d''][cis''8 a''] a''4^\prall~
%3
[a''16 g'' f'' e''][d'' c'' bes' a'][bes'8 \lh g''] g''4^\prall~
%4
[g''16 f'' e'' d''][c'' bes' a' g'][a'8 \rh f''] f''4^\prall~
%5
[f''16 e'' d'' c''][bes' a' g' f'][g'8 \lh e''] e''4^\prall~
%6
[e''16 d'' cis'' b'][a' g' f' \rh e'] [f' d' g' e'][a' f' e' d']|
%7
[cis'16 a d' b][e' cis' f' d'][g' e' a' f'][b' g' cis'' a']|
%8
[d''16 b' e'' cis''][f'' d'' g'' e''] a''4 g''^\prall|
%9
\lh a''4 g''^\prall \rh a'' g''^\prall|
%10
[f''16 d'' g'' e''][a'' f'' e'' d''] [cis'' a' d'' b'][e'' g' f' e']|
%11
[f'16 d' g' e'][a' f' e' d'][cis' a d' b][e' cis' f' d']|
%12
[g'16 g' f' e'][f'8 g'16^\prall f'32 g'][a'16 e' f' d'][e' cis' \lh f'8]|
%13
[e'8 d'] cis'16 s8. s2 \bar "||";
%14
\rh [a'16 b' cis'' d''][e'' cis'' f'' d''] e''4. a'8|
%15
[b'16 g' cis'' a'][d'' b' e'' cis''][fis'' d'' g'' e''][a'' c'' bes' a']|
%16
[bes'16 g' c'' a'][d'' bes' a' g'][fis' d' g' e'][a' c' bes a]|
%17
\lh [bes16 g c' a][d' bes e' c'][f' d' g' e'][a' f' bes' g']|
%18
\rh c''4 bes'^\prall \lh c'' bes'^\prall|
%19
\rh [c''8 f''][bes' e''][a'16 f' c'' a'][f'' c'' a'' f'']|
%20
[e''16 g'' c'' e''][g' c'' bes' a'] \lh [bes' g' d'' bes'][g'' d'' bes'' g'']|
%21
[f''16 a'' d'' f''][a' d'' c'' b'] \rh [c'' a' e'' c''][a'' e'' c''' a'']|
%22
[gis''16 b'' e'' gis''][b' d'' cis'' b'][cis''8 a''] a''4^\prall^"\\textflat"~
%23
[a''16 g'' f'' e''][d'' c'' \lh bes' a'] [bes'8 \rh g''] g''4^\prall~
%24
[g''16 f'' e'' d''][c'' bes' \lh a' g'][a'8 \rh f''] f''4^\prall~
%25
[f''16 e'' d'' c''][bes' a' \lh g' f'][g'8 \rh e''] e''4^\prall~
%26
[e''16 d'' cis'' b'][a' g' f' e'] [f' d' g' e'][a' f' bes' g']|
%27
\lh [f'16 d' g' e'][a' f' bes' g'] \rh [f' d' g' e'][a' f' bes' g']|
%28
[a'8 d''][g' cis''] \lh [a' f''][g' e'']|
%29
\rh [a'8 d''][g' e''][f'16 d' g'16 e'][a' f' b' g']|
%30
[cis''16 a' d'' e''][f'' d'' e'' cis''][d'' a' bes' g'][a' f' \lh bes'8]|
%31
[a'8 g'] f'16 s8. s2 \bar "|.";
}
\context Thread=tTwo{
s1*9|
%10
d''16 s4.. a'16 s4..|
%11
d'16 s2...|
s1*3|
%15
s2 d''16 s4..|
%16
g'16 s4.. d'16 s4..|
s1*2|
%19
s8 \rh a'' s g'' s2|
s1*8
%28
s8 f'' s e'' s d'' s cis''|
%29
s8 f'' s cis''
}>
\context ThreadedVoice = vTwo <
\context Thread=tThree{
\stemdown \slurdown
%1
\lh s2 [d'16 e' f' g'][a' a cis' a]|
%2
[d'8 e' f' g'][a' a' a' a']|
%3
[a'8 a' a' f'][g \rh bes' bes' bes']|
%4
[g'8 g' g' e'][f \lh a' f' a']|
%5
[d'8 f' d' d'][e \rh g' g' g']|
%6
[e'8 e' e' a] [d' \lh e f g]|
%7
a8 s [cis' d'][e' f'][g' a']|
%8
[b'8 cis''][d'' e''][f''16 d'' bes' g'][e'' cis'' a' cis'']|
%9
\rh [f''16 d'' bes' g'][e'' cis'' a' cis''] \lh [f'' d'' bes' g'][e'' cis'' a' cis'']|
%10
[d'8 e' f' g'][a b cis' a]|
%11
[d8 e f g][a b][cis' d']|
%12
[e16 e' d' cis'][d'8 bes] \stemup a,4 \stemdown s8 \rh [d'16 b]|
%13
[cis'16 a b gis][a16 e cis e] \lh \stemup a,4^\prall^"\\textnatural" r^\fermata|
%14
\stemdown s2 [a16 b cis' d'][e' cis' f' d']|
%15
[g'8 a'][b' cis''][d' e' fis' d']|
%16
[g8 a bes c'][d e fis d]|
%17
\rh [g8 a][bes c'][d' e'][f' g']|
%18
\lh [a'16 f' d' bes][g' e' c' e'] \rh [a' f' d' bes][g' e' c' e']|
%19
\lh [a'16 f' d' bes][g' e' c' e'][f'8 f f f,]|
%20
[c8 c' c' fis'] \rh [g' g g g,]|
%21
[d8 d' d' gis'] \lh [a' a a a,]|
%22
[e8 e, e, e,][a, a' a' a']|
%23
[f,8 a' a' f'][\rh g, \lh bes' bes' bes']|
%24
[e,8 e' g' e'][\rh f, \lh a' a' a']|
%25
[d,8 f' f' d'][\rh e, \lh g' g' g']|
%26
[a,8 a e' a] [d' e'][f' g']|
%27
[d'8 e'][f' g'] \rh [d' e'][f' g']|
%28
\lh [f'16 d' bes g][e' cis' a cis'] \rh [f' d' bes g][e' cis' a cis']|
%29
\lh [f'16 d' bes g][e' cis' a cis'][d8 e][f g]|
%30
[a8 g a a,] \stemup d4 \stemdown s8 \rh [g'16 e']|
%31
[f'16 d' e' cis'][d'16 a f a] \lh \stemup d4^\prall r4^\fermata|
}
\context Thread=tFour{
s1*2|
%3
\lh f'8 f' f' s4 \rh g'8 g' g'|
%4
e'8 e' e' s4 \lh f'8 a' f'|
%5
f'8 d' f' s4 \rh e'8 e' e'|
%6
a8 a a s d s4.|
s1*16|
%23
\lh s8 f' f' s4 g'8 g' g'|
%24
s8 g' e' s4 f'8 f' f'|
%25
s8 d' d' s4 e'8 e' e'|
%26
s8 e' a s s2|
s1*2|
%29
s2 d'8
}>
\context ThreadedVoice = vThree <
\context Thread=tFive{
\stemdown \slurdown
s1*25|
%26
\stemup s2 \rh d''4 s|
%27
\lh d''4 s \rh d''4
}>
>}
\paper{barsize=50.0; indent=0.0;
\translator {\VoiceContext beamAuto=0; \remove Auto_beam_engraver;}
\translator {\StaffContext \consists Bar_number_engraver;}
  \translator {\VoiceContext  \remove Tuplet_engraver;}
}}
