%K000.ly LilyPond 1.2.16

\version "1.3.117"

\include "scarlatti-properties.ly"
\include "scarlatti-paper.ly"
%sonata-specific settings
\paper {
\translator{\VoiceContext
  %% huh?
  %%beamAutoBegin=0;
  autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 3 8)
  autoBeamSettings \override #'(end 1 16 * *) = #(make-moment 3 8)
  autoBeamSettings \override #'(end 1 24 * *) = #(make-moment 1 8)
  autoBeamSettings \override #'(end 1 32 * *) = #(make-moment 1 8)
}
}

%{
%standard properties list
rh={\property Thread.noteHeadStyle=""}
lh={\property Thread.noteHeadStyle="diamond"}
n={\property Thread.fontSize="0"}
sm={\property Thread.fontSize="-1"}
su={\property Voice.verticalDirection="1"}
sd={\property Voice.verticalDirection="-1"}
zs={\property Voice.forceHorizontalShift="0.0"}
ls={\property Voice.forceHorizontalShift="-0.6"}
sls={\property Voice.forceHorizontalShift="-0.22"}
rs={\property Voice.forceHorizontalShift="0.6"}
srs={\property Voice.forceHorizontalShift="0.22"}
ab={\property Voice.noAutoBeaming=##f}
xb={\property Voice.noAutoBeaming=##t}
%}


\score{
\context PianoStaff \notes<
\context Staff=up<
  \key d \major ;
\time 3/8;
\clef treble;
\autochange Staff
\context Voice=va< \zs
\su
\context Thread=ta{ \n
\rh \sm[\times 2/3{a'32_"John's Scarlatti quotes" d'' fis''}\n a''16]a'8 g'~
%a2
g'8 _"1 Aug 2000" \times 2/3{a'16 b' cis'' d'' fis'' g''}|
%a3
a''8 a' a'|
%a4
a'8 s \lh d''|
%a5
s8 \rh fis'' s|
%a6
a''32 s fis'' s d'' s a' s d' s a s \bar "||";
\key f \major; 
%a7
r16 a' f'8. d'16~
%a8
d'16 g' e'8. cis'16|
%a9
r16 a' f'8. d'16~
%a10
d'16 g' e'8. cis'16 \bar "||";
\key d \major;
%a11
r16 fis' d'8. g'16~
%a12
g'16 fis'8 e' b'16~
%a13
b'16 a' fis'8. d''16~
%a14
d''16 cis'' a'8. fis''16~
%a15
\sd fis''16 e''8 cis'' a'16~
%a16
\su a'8 d'' b''16 s|
%a17
\lh \sd r16 e''8 cis'' a'16~
%a18
\su a'8 d'' b''16 s|
%a19
s8 a''^"M" s|
%a20
\rh e''4.|
%a21
\lh s8 fis'' s|
%a22
s8 b''^"M" a''|
%a23
gis''8 \rh[b' c'']|
%a24
gis'8_\prall f''32 d'' ais' b' e'' c'' gis' a'|
%a25
d''32 b' fis' gis' c'' a' dis' e' b' gis' dis' e'|
%a26
a'8~a'32 gis' a' b' c'' gis' a' b'|
%a27
c''8 b'4~
%a28
b'8 a' gis'|
%a29
r8 a'4~
%a30
a'8 cis''4^\prall~
%a31
cis''4._\prall~
%a32
cis''4._\prall
%a33
s8 gis''4|
%a34
\times 12/15{a''16 \sm gis''32 fis'' e'' d'' cis'' b' a' gis' fis' e' d' cis' b \n}
%a35
s32 e' cis' a s a' e' cis' s cis'' a' e'|
%a36
\lh a'32 \rh[e'' cis'' a']\lh cis'' \rh[a'' e'' cis'']\lh a' \rh[a cis' e']\bar "||";
\time 6/8;
%a37
gis''8 a'' gis'' a'' gis'' a''|
}
 \context Thread=tb{ \n
 \rh s8 fis' e'~
 %a2
 e'8 s4|
 %a3
 s8 fis' g'|
 %a4
 fis'8 s \lh a'|
 %a5
 s8 \rh d'' s|
 s4.*3
 %a9
 s8 \sm g' \n s|
 %a10
 s8 \sm fis' \n s|
 %a11
 s4 s16 e'~
 %a12
 e'16 s8 cis' s16|
 s4.*8
 %a21
 s8 \lh a' \rh s|
 s4.
 %a23
 s8 gis' a'|
 %a24
 b'8^\prall s4|
 s4.*3
 %a28
 s4 e'8|
 s4.*2
 %a31
 s8 e''4^\prall~
 %a32
 e''4.^\prall|
 s4.*4
 %a37
 s4. c''8 b' c''
 }
  \context Thread=tc{ \n
  \rh s8 d' a'~
  %a2
  a'8 s4|
  %a3
  s8 d' dis'
  %a4
  d'8 s \lh fis'|
  %a5
  s8 \rh a'
  }>
\autochange Staff
\context Voice=vb< \zs
\sd
\context Thread=td{ \n
\lh \sm[\times 2/3{d32 fis a}\n d'16]a8 a|
%b2
d4 r8|
%b3
r8 a a|
%b4
r32[d fis a][d' fis' a' d'']r \rh[d fis a]
%b5
[d'32 fis' a' d'']r32 \lh[d fis a][d' fis' a' d'']
%b6
s a''32 s fis'' s d'' s a' s d' s a|
%b7
\su r8. f16 bes8|
%b8
r8. g16 a8~
%b9
a16 r8 f16 bes8|
%b10
bes8 r16 g bes8|
s4.
%b12
r8 a b|
%b13
\sd r8 d'4|
%b14
e'8 fis'4|
%b15
\su s8 a''^"M" s|
%b16
\sd b8 d' gis'|
%b17
\rh \su s8 a'' s|
%b18
\sd b8 d' gis'|
%b19
\times 2/3{a'16 c'' b' a' c'' d'' e'' c'' b'}|
%b20
\ls c''8^\prall \zs b'4|
%b21
\times 2/3{s16 c'' b' s b' c''}dis''32 c'' b' a'|
%b22
b'32 gis' dis' e' d''8 cis''|
%b23
b'32 gis' dis' e' s4|
%b24
s8 \lh d' cis'|
%b25
b8 a gis|
%b26
a32 d' c' b c' \rs f' \zs e' d' c' e' fis' gis'|
%b27
\su a8 b \sd d'~
%b28
d'8 c' \su b|
%b29
a8 s b|
%b30
[a8 \sd a' \su b]|
%b31
[a8 \sd a' \su b]|
%b32
[a8 \sd a' \su fis]|
%b33
gis'8 \sd e'' \su gis'|
\sd a'8 s4|
%b35
\xb a8 cis' e' \ab|
s4.
%b37
\rh \su \srs e''16 dis'' e'' dis'' e'' dis'' \lh \sd \sls e''16 dis'' e'' dis'' e'' dis'' \zs|
}
 \context Thread=te{ \n
 \lh s4.*24
 %b25
 b,8 a, gis,|
 s4.*3
 %b29
 s4 d'8|
 %b30
 cis'8 s d'|
 %b31
 cis'8 s d'|
 %b32
 cis'8
 }>
\autochange Staff
\context Voice=vc< \zs
\sd
\context Thread=tf{ \n
\lh s4.*2
%c3
s4 \rs c'8 \zs|
s4.*3
%c7
d4.|
%c8
e4.|
%c9
d4.|
%c10
e4.|
%c11
d8 fis cis|
%c12
d4 g8|
%c13
fis4 b8|
%c14
a4 d'8|
%c15
a8 s a_"M"|
%c16
s4 s16 \rh e|
%c17
a8 s a|
%c18
s4 s16 \lh e|
%c19
a8_"M" s a|
%c20
a'4 gis'8|
%c21
dis8 s dis|
%c22
e8 s4|
%c23
s8 e dis|
%c24
e8 d cis|
s4.*2
%c27
e4.|
%c28
d8 e4|
s4.*4
%c33
e8 e' e|
s4.*3
%c37
b'8 c'' b' e' e' e'
}
 \context Thread=tg{ \n
 \lh s4.*10
 %c11
 s4 a8|
 s4.*25
 %c37
 e'8 e' e'
 }>
\autochange Staff
\context Voice=vd< \zs
\sd
\context Thread=th{ \n
\rh s4.*20
%d21
a'4 s8|
s4.*6
%d28
\su \srs f'4 \zs
}>
>
\context Staff=down<
\clef bass;
\key d \major;
s4.*38
>>
} 

