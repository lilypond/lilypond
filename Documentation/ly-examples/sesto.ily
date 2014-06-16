%%% G.F Haendel, Giulio Cesare in Egitto
%%% Act I, scene IV
%%% Sesto: Svegliatevi nel core, furie d'un alma offesa (excerpt)
%%%
%%% Nicolas Sceaux <nicolas.sceaux@free.fr>

\version "2.19.2"
\header {
  title = "Giulio Cesare in Egitto"
  subtitle = "Sesto: Svegliatevi nel core, furie d'un alma offesa (excerpt)"
  composer = "G.F Handel"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Notes and lyrics
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sesto = {
  R1*6 |
  r4 r8 g' c''  ees''16[ d''] c''8 c'' |
  c'' g' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r c'' c''  b'16[ c''] d''8 g' |

%{
  %%10
  f'4 r8 d'' ees''  d''16[ c''] b'4 |
  c'' r8 c'' c''8. c'16 c'4 |
  r r8 c'' aes'  c''16[ bes'] aes'8 g' |
  f' f' r4 bes'8 bes'16 aes' g'8 f' |
  ees' ees' r ees'' ees''  d''16[ ees''] f''8 bes' |
  %%15
  aes'4. bes'8 g' ees'' f'4 |
  ees' r8 ees'' ees''8. ees'16 ees'4 |
  r r8 c'' c''8. c'16 c'4 |
  r r8 c'' c''  d''16[ ees''] f''8 ees'' |
  d'' bes' r4 bes'8 c''16 d'' ees''8 d'' |
  %%20
  c'' c'' r c'' f'  f''16[ ees''] d''8 c'' |
  b'16[ a'] g'8 r4 ees''8 ees''16 d'' c''8 bes' |
  aes' g' r g' aes' f' d'' b' |
  f''2 ~ 8 d'' b' g' |
  ees'4 r8 d'' ees''  d''16[ c''] b'4 |
  %%25
  c'' r8 g' c'' g' ees' g' |
  c'4 r8 g' aes' f' d'' b' |
  f''2 ~ 8 d'' b' g' |
  ees'4 r8 g'' ees''  d''16[ c''] b'4 |
  c''2 r |
  %%30
  R1*5 |
%}
}

sestoLyrics = \lyricmode {
  Sve -- glia -- te -- vi nel co -- re,
  fu -- rie "d'un" al -- "ma of" -- fe -- sa,
%{
  a far "d'un" tra -- di -- tor a -- spra ven -- det -- ta!
  sve -- glia -- te -- vi,
  sve -- glia te -- vi nel co -- re,
  fu -- rie d'un al -- "ma of" -- fe -- sa,
  a far "d'un" tra -- di -- tor as -- pra ven -- det -- ta,
  sve -- glia -- te -- vi,
  sve -- glia -- te -- vi,
  sve -- glia -- te -- vi nel co -- re,
  fu -- rie d'un al -- "ma of" -- fe -- sa,
  sve -- glia -- te -- vi nel co -- re,
  fu -- rie d'un al -- "ma of" -- fe -- sa,
  a far d'un tra -- di -- tor, __
  d'un tra -- di -- tor as -- spra ven -- det -- ta,
  a far d'un tra -- di -- tor,
  a far d'un tra -- di -- tor, __
  d'un tra -- di -- tor as -- pra ven -- det -- ta!
%}
}

violinoI = {
  r4 r8 g' c'' ees''16 d'' c''8 c'' |
  c'' c' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r c'' c''8.^\trill b'32 c'' d''8 g' |
  f'4. d''8 ees'' d''16 c'' b'4 |
  %%5
  c''16 g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' |
  c'' g'' f'' g'' ees'' g'' f'' g'' aes''8 g''16 f'' f''4^\trill |
  ees'' r8 g'_\p c'' ees''16 d'' c''8 c'' |
  c'' g' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r c'' c'' b'16 c'' d''8 g' |
%{
  %%10
  f'4 r8 d'' ees'' d''16 c'' b'4 |
  c''16 g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' |
  ees'' g'' f'' g'' ees'' g'' f'' g'' aes''8 aes' r4 |
  d''16 f'' ees'' f'' d'' f'' ees'' f'' g''8 g' r4 |
  ees'16 g' f' g' ees'8 ees'' ees'' d''16 ees'' f''8 bes' |
  %%15
  aes'4. bes'8 g' ees'' d''4 |
  ees''16 bes'' aes'' bes'' g'' bes'' aes'' bes'' g'' bes'' aes'' bes'' g'' bes'' aes'' bes'' |
  g'' g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' |
  ees'' g'' f'' g'' ees'' g'' f'' g'' aes'' c'' c'' bes' aes' c'' bes' c'' |
  d'' f'' ees'' f'' d'' f'' ees'' f'' g'' bes' bes' aes' g' bes' aes' bes' |
  %%20
  c'' ees'' ees'' f'' g'' ees'' f'' g'' aes'' f'' g'' ees'' f'' d'' ees'' c'' |
  \tag #'violin { g'' g' g'' g'' } \tag #'reduction { g'' d'' g'' g'' } g'' b' g'' g'' g'' f'' ees'' d'' c''8 bes' |
  aes' g' r g' aes' f' d'' b' |
  f''2 ~ 8 d'' b' g' |
  ees'16 c' g' ees' c'' g' g'' d'' g'4 r |
  %%25
  c'''16 ees''' d''' ees''' c''' ees''' d''' ees''' g'' c''' b'' c''' g'' c''' b'' c''' |
  ees'' g'' f'' g'' ees'' g'' f'' g'' \tag #'violin { aes'8 f' } \tag #'reduction { c''8 aes' } d'' b' |
  f''2 ~ 8 d'' b' g' |
  ees'4^\fermata r r2 |
  r4 r8 c'_\f c'' ees''16 d'' c''8 c'' |
  %%30
  c'' c' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r g' aes' f' d'' b' |
  f''2 ~ 8 d'' b' g' |
  c''16 g'' f'' g'' ees'' g'' f'' g'' c'' g'' f'' g'' ees'' g'' f'' g'' |
  b'8 c''16 d'' ees''8 d'' c''4^\fermata r
%}
}

violinoII = {
  r4 r8 g' c'' ees''16 d'' c''8 c'' |
  c'' c' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r c'' c''8.^\trill b'32 c'' d''8 g' |
  f'4. d''8 ees'' d''16 c'' b'4 |
  %%5
  c''16 ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' |
  c'' ees'' d'' ees'' c'' ees'' d'' ees'' d''8[ ees''] ees'' d'' |
  ees''4 r8 g' c'' ees''16 d'' c''8 c'' |
  c'' g' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r c'' c'' b'16 c'' d''8 g' |
%{
  %%10
  f'4 r8 d'' ees'' d''16 c'' b'4 |
  c''16 ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' |
  c'' ees'' d'' ees'' c'' ees'' d'' ees'' f''8 f' r4 |
  bes'16 d'' c'' d'' bes' d'' c'' d'' ees''8 ees' r4 |
  c'16 ees' d' ees' c'8 ees'' ees'' d''16 ees'' f''8 bes' |
  %%15
  aes'4. bes'8 g' ees'' d''4 |
  ees''16 g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' ees'' g'' f'' g'' |
  ees'' ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' |
  c'' ees'' d'' ees'' c'' ees'' d'' ees'' f'' aes' aes' g' f' aes' g' aes' |
  bes' d'' c'' d'' bes' d'' c'' d'' ees'' g' g' f' ees' g' f' g' |
  %%20
  aes' c'' c'' d'' ees'' c'' d'' ees'' f'' d'' ees'' c'' d'' b' c'' a' |
  \tag #'violin { b' d' d' b' } \tag #'reduction { b' g' d'' b' } b' g' b' d'' g'' f'' ees'' d'' c''8 bes' |
  d' ees' r g' aes' f' d'' b' |
  f''2 ~ 8 d'' b' g' |
  ees'16 c' g' ees' c'' g' g'' d'' g'4 r |
  %%25
  ees''16 g'' f'' g'' ees'' g'' f'' g'' ees'' ees'' d'' ees'' c'' ees'' d'' ees'' |
  g' ees'' d'' ees'' c'' ees'' d'' ees'' \tag #'violin { c''8 aes' } \tag #'reduction { aes'8 f' } f' d' |
  f''2 ~ 8 d'' b' g' |
  ees'4^\fermata r r2 |
  r4 r8 c' c'' ees''16 d'' c''8 c'' |
  %%30
  c'' c' ees''4 ~ 8 d''16 ees'' f''8 aes' |
  aes' g' r g' aes' f' d'' b' |
  f''2 ~ 8 d'' b' g' |
  c''16 ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' c'' ees'' d'' ees'' |
  d'8 ees'16 f' g'8 b' c''4^\fermata r
%}
}

bassi = {
  c4 r8 g c' ees'16 d' c'8 c' |
  c' c r g aes f bes bes, |
  ees ees, r c ees c g ees |
  aes f d' b c' f g g, |
  %%5
  c4 r c'8 c bes bes, |
  aes aes, g g, f ees bes bes, |
  ees4 r r r8 c_\p |
  c' ees'16 d' c'8 bes aes f bes bes, |
  ees ees, r c ees c g ees |
%{
  %%10
  aes f d' b c' f g g, |
  c4 r r r8 c' |
  c'8. c16 c4 r8 f d ees |
  bes bes, r bes g aes bes g |
  c' c c' bes16 aes g8 ees bes g |
  %%15
  c' c d bes, ees g bes bes, |
  ees4 r r r8 ees' |
  ees'8. ees16 ees4 r r8 c' |
  c'8. c16 c8 c' aes f r f |
  bes bes, bes, bes g ees r ees |
  %%20
  aes aes, g g, f f, r f |
  g g, r4 c'8 g aes g |
  f ees r4 r r8 g, |
  aes, f, d b, aes f d' b |
  c'4 r8 b c' f g g, |
  %%25
  c' g c g, c,4 r |
  c'8 g c c' f4 r8 g, |
  aes, f, d b, aes f d' b |
  c'4 r8 b c' f g g, |
  c4 r8 c_\f c' ees'16 d' c'8 c' |
  %%30
  c' c r g aes f bes bes, |
  ees g ees c f,4 r8 g, |
  aes, f, d b, aes f d' b |
  c' c bes bes, aes aes, g g, |
  f c g g, c4^\fermata r
%}
}

global = {
  \key c \minor
  \set Score . tempoWholesPerMinute = #(ly:make-moment (/ 80 4) 1 0 1)
  \time 4/4
%s1*34 \bar "|."
}
