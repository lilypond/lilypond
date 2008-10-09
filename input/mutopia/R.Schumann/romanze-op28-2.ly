% NOT FINISHED!!!!

\include "deutsch.ly"

#(set-global-staff-size 16)

\version "2.11.61"

\header {
  title = "Romanzen"
  opus = "op. 28/2"
  composer = "Robert Schumann (1810-1856)"
  enteredby="Rune Zedeler"
  maintainer="rune@zedeler.dk"
  mutopiatitle = "Romanzen - op. 28/2"
  mutopiacomposer = "R. Schumann 1810-1856"
  mutopiainstrument = "Piano"
  style = "Romantic"
  copyright = "Public Domain"
}

u = { \change Staff = up  \stemDown }
m = { \change Staff = mid  \stemUp  }

%
% d = .. complains about note names.
% 
#(define d  #{ \change Staff = down  \stemUp #})

forcedBreak = \break

global =  { \key fis \major \time 6/8
  \set Score.beatLength =  #(ly:make-moment 3 8)
  \repeat volta 2 { s2.*8 } s2.*26 \bar "|."
}

righta =  \transpose c cis' {
 % \stemUp \slurUp \tieUp
 \stemUp
 \repeat volta 2 {
  \override TextScript   #'extra-offset = #'(-8.0 . 2.5)
  \m  a,16[^\p( \u c^\markup {
      \large "Einfach ("
      \note #"8" #1
      \large " = 100)" }
  a c ] \m  g,[ \u c^3 ] \m  b,[ \u c^2 b c] \m  a,[ \u c^3]) | 
  \revert TextScript #'extra-offset
  \m  f,[( \u c f c] \m  g,[ \u c^4] \m  a,[ \u c^2 a c8  c16)] | 
  \m  c16[( \u f c' f] \m  b,[ \u f] \m  d[ \u f^3 d' f^2] \m  c[ \u  f)^4] |
  \m  f,16[( \u c16^3 f c] \m  g,[ \u c^4] \m  a,[ \u c^2 a c8  c16)] |
  \m  a,[( \u c a c] \m  c[ \u d c' d] \m  b,[ \u d] \m  a,[ \u  d)^3] |
  \m  g,[( \u c^2 g c] \m  b,[ \u c b c] \m  a,[ \u c] \m  g,[ \u  cis)^3] |
  \m  f,[( \u d^3 f d] \m  g,[ \u d g d] \m  g,[ \u c] \m  a,[ \u  a)] |
  \m  g,[( \u c g c] \m  f,[ \u h,^2 g h,] \m  e,[ \u c^2 g  c)] |
 }
 \forcedBreak
 \stemUp \slurUp \tieUp
 b4^5( <a g>8~<a g> a g^4 |
 f4^5 e8^5~e  d4)^4 |
 as4^4( <g f>8~<g f> g^5 f^4 |
 es4^5 d8^5~d  c4)^4 |
 c^4( es8^5~<es as,_1> d^3  f) |
 e4^4( g8^5~<g ces_4> f^3  as) |
 g4^4 b8^5~b a c'^5~ |
 c' b^4 ges^5 des^3 b,^5 \d  ges,16[^4 f,] |
 e,^2^\markup { \large "ritard..." } cis,^1 \u r8 r r4 r16 \d gis,^2^"R.H."] |


 \forcedBreak
 \m  a,16[^\p( \u c a c] \m  g,[ \u c^3] \m  b,[ \u c^2 b c] \m  a,[ \u  c)^3 ]| 
 \m  f,[( \u c f c] \m  g,[ \u c^4] \m  a,[ \u c^2 a c8  c16)] | 
 \m  c16[( \u f c' f] \m  b,[ \u f] \m  d[ \u f^3 d' f^2] \m  c[ \u  f)]^4 | 
 \m  f,[( \u c^3 f c] \m  g,[ \u c^4] \m  a,[ \u c^2 a c8  cis16)] |
 \m  a,[( \u d^2 a d] \m  c[ \u d c' d] \m  b,[ \u  d)^3] r8 |
 \m  c16[( \u f c' f] \m  es[ \u f es' f] \m  d[ \u  f)^3] \m  f[( \u as^2] |
  f'[  as)] \m  f[( \u as f'  as)] r8^\fermata r16 e'^2( f' g' | \stemNeutral \tieNeutral
 as' des'^1 c'8^\markup { \finger "2-3" } h~h  c'16) \clef F  e,16[(^2 f, g,] |
 \forcedBreak

  as,[^5 h,,^2)] \clef G <c a^3>8[( <c g^2>16 c'] <c es ges c'^5>4.~
 <c es ges c'>8) s4 s8 r16 h^2( c' d' | 
 es' ges-1 f-2 a-1 c'-2 f'-4 \stemUp \tieUp e'-5 d' c' b-4 a^\prall^\markup { \finger "2-4-3" } g |
  f) f'-5( e'-5 d'-4 c'-3  f')-5 <f^4 a^5>4( <e^3 g^5>8 |
 <a, f^4>4 <g, e^3>8 f16^4 d'^5~ d'8.[ c'16^4] |
 \forcedBreak 
 << f8)^3_\pp \new Voice =  "another" { \m  a,16[ \u c8 c16] } >> \m  g,[ \u c_3] \m  b,[^\markup { \finger "2-1" } \u c8 c16] \m  a,[ \u c]~ | \stemNeutral
 c16 c8_4 c c16~c c8_2 c c16~ |
 c16 c8 c c16~c c8 c c16~ |
 c16 c8 c c16 r4^\fermata r8 | \bar "|."

}

rightb =  \transpose c cis' {
 \relative c { \stemDown \slurDown
  \repeat volta 2 {
   a4^1( g8^1 b4^1 a8^1 |
   f8..^1 f32^1 g8^2  a4.)^1 |
   c4^1( b8 d4 c8 |
   f,8.. f32^1 g8^2  a4.) |
   a4(^1 c8^1_\accent~c b^1 a^1 |
    g4) b8^1_\accent(~b a^1 g^1 |
    f4)^1 g8_\accent(~g g_. a_. |
   g4 f8~f  e4) |
  }
 }
 \u \slurUp
 g16_4 d b, d g e cis g_4 f_3 d_1 e_2 b, |
 d_3 a,_2 f, a, cis_4 a, e, a, d a,8 g,16 |
 f_4 c as, c f d h, f_4 es_3 c_1 d_2 as, |
 c_3 g,_2 es, g, h,_4 g, d, g, c g,8 f,16 |
 c g, es, g, es c_3 as, c^2 d^3 c h, c |
 e des b,\< des g e_3 ces_4 d_2 f^3 d cis d |
 \once \override PhrasingSlur   #'extra-offset = #'(0 . 3)
 g\( fes des fes b g_3 es_1\! ges_2 a^3 ges f_1 ges_2 |
 r ges_2 b des_1 ges b,_2 des ges,^1 b, \d des,^1 \stemDown \transpose c' c { b,[_1 as,] |
 g,8  b,16[ g, e, cis,]  d,\)_4 \< f, h, d_3 f_2  r16\! }

 \change Staff=mid
 \relative c { \stemDown \slurDown
  a4^1( g8^1 b4^1 a8^1 |
  f8..^1 f32^1 g8^2  a4.)^1 |
  c4^1( b8 d4 c8 |
  f,8.. f32^1 g8^2  a4.) |
  a4^1( c8^1_\accent~[c b)] g16^1([ b^2] |
   c4)^1 es8^1~es d^1 f^1~ |
  \override PianoStaff.Arpeggio  #'direction = #UP
  f f8.. f32^1( as4.)^\fermata\arpeggio ~ |
  \revert PianoStaff.Arpeggio #'direction
  \stemUp \tieUp as r4 r8 |
 }
  s2. s
 \u s4. \grace {
  \override Stem   #'stroke-style = #"grace"
  f8(
  \revert Stem #'stroke-style }
  f4) e8 |
 f g16_2 b_1 a_2 gis_1 c_2 h, c cis_1 d_2 b,_1 |
 \tieDown c4.~<f, c  >16\< \tieNeutral <f h>~ < f^3 h >8[\>\! < e b^2> \!] |
 \change Staff=mid
 \transpose c' c {
  a4^1( g8^1 b4 a8^2 |
  f8..^1 f32^1 g8^2  a8..)   \slurUp <f f'^3>32( <g g'^4>8 |
  <a a'^5>8..) <f f'^3>32( <g g'^4>8 <a a'^5 >4.\>) ~ |
  a~ a\!^\fermata \bar "|."
 }
}

lefta =  \transpose c cis {
 \stemUp \slurUp \tieUp
 \repeat volta 2 {
  f4^1( e8^1 g4^1 f8^1 |
  d8..^1 d32^2 e8^1  f4.) |
  a4^1( g8 b4 a8 |
  d8..^1 d32^2 e8^1  f4.) |
  fis4^1 a8^1^\accent(~a g^1 f^1 |
   e4)^1 g8^1^\accent(~g f^1 e^1 |
   d4)^1 f8^1^\accent(~f e^. dis^1^. |
  e4^1 d8^1~d  c4)^1 |
 }
 \d \tieNeutral \slurDown
 <<
 \transpose c' c {
  g4 a8~a f g | a4 g8~g f e |
  d4 g8~g es f | g4 f8~f es d |
  c4 r8 f4 as8~ | as g b as4 ces'8~ |
  ces' b des' c'4 \new Voice =  "another" { \stemUp \tieUp <es' es>8~ |
  es' des'4^1~ \stemDown  des'8.[ c'16] }
 }
 \transpose c' c, {
  g4(-4 a8-3~a f-5 g-4 | a4-3 g8-\markup { \finger "4-3" } ~g f-4 e-5 |
   d4)-\markup { \finger "4-5" }( g8-3~g es f-4 | g4-3 f8-4~f es d-4 |
   c4) r8 f4-4( as8-5~ | as g-4  b)-5 as4-5( ces'8-4~ |
  ces' b des'-4  c'4)-5( <es'-4 es''>8 |
  \stemDown \tieDown  des'4.)_5~des'~ |
  \override NoteColumn   #'horizontal-shift = #-1 des' s
 }
 >>
 \change Staff=down \stemUp \slurUp \tieUp \phrasingSlurUp
 f4^1( e8^1 g4^1 f8^1 |
 d8..^1 d32^2 e8^1  f4.) |
 a4^1( g8 b4 a8 |
 d8..^1 d32^2 e8^1  f4.) |
 fis4 a8^\accent~a g s |
 a4 c'8^\accent(~c'  b) d'^\accent~ |
 d' d'8..^\accent d'32-1 s4. |
 s2.*2
 s8 r16 h\( c' d' es' as g8\arpeggio  fis(
 ges)\) f16-2( a-1 c'-3  f')-1 \grace {
  \override Stem   #'stroke-style = #"grace"
  \stemDown \slurUp  b,[( f] \stemUp 
  \revert Stem #'stroke-style }
  e')-1( d' c' b-1 a-2\prall g 
  f16)-4 f' <c' e'> d'-1 c'-2 h-1 s4.
 s2.
 f4^1( e8 g4 f8 |
 d8..^1 d32-2 e8^1  f8..) d32^2( e8^1 |
  f8..) d32-2( e8^1  f4.)^1 ~ |
 f~ f\fermata |
}

leftb =  \transpose c cis {
 \stemDown \slurDown \tieDown
 \repeat volta 2 {
  f16^\p c f, c e c-3 g c-2 f, c f c |
  d c f, c-3 e c f c f, c8 c16-3 |
  a16 c f, c g c-3 b c f, c a c |
  d c f, c-3 e c f c-2 f, c8 c16 |
  fis d-2 d, d  a[ d] g, d g d f h,-3 |
  e c-2 c, c  g[ c] f, c f c e a,-3 |
  d a, d, a,  f[ g,-2] h,, g, e c, dis fis,-5 |
  e c g,-4 c d g,-2  c,[ g, c g,] c,8 |
 }

 s2.*9 |

 f16^\p c c, c e c-3 g c-2 c, c f c |
 d c c, c-3 e c f c f, c8 c16-3 |
 a16 c f, c g c-3 b c f, c a c |
 d c f, c-3 e c f c-2 f, c8 cis16 |
 fis^1 d d, d  a[ d_2] g, d g d_3  b,[_5( g_2] |
  a) c_3 f, c  c'[ f_2] b, f b f_3 d' as |
 h, as d' as h, as( f'4.)^\fermata\arpeggio_2 ~
 \d \stemDown f'8. \clef G \stemNeutral \tieNeutral \phrasingSlurDown
   \transpose c c' { e16[_4( f g]  as[ des_3]  c8[_4 <h, d f d'>)]~ |
 <h, d f d'>} \clef F <c_3 f_1>[( <b,_4 e^2>] <a,_5 eis^1>8.) h,16_4\( c d |
 \voiceTwo
 es as,_3 g,8 fis,~fis, g,_4\arpeggio gis,_5~ |
 gis, <a, c>16\) r16 r8 r4 cis8( |
  d) e( f16_4 d_5  c4.)_ \markup { \finger "4-3" }~ |
 \oneVoice
 c16( h,_4 c_3 cis_1 d_2 b,_3 a,_4 as,_1 g,8  c,8) |
 \change Staff = down \stemDown \tieDown
 f16^\pp c f, c e c-3 g c-2 f, c f c |
 d c f, c-3 e c f c-2 f, c-3 e c |
 f c-2 f, c-3 e c ~ c c8 c c16 ~ |
 c c8 c c16 r4_\fermata r8 \bar "|." |
}


\paper {
    indent = 0.5 \in
    % textheight = 29.8 \cm
    pagenumber = no
    line-width = 17.0 \cm
}  
		       
\score { 
  \context PianoStaff <<
    #(set-accidental-style 'piano-cautionary)
    \override PianoStaff.NoteCollision   #'merge-differently-dotted = ##t
    \set PianoStaff.connectArpeggios = ##t
    \override PianoStaff.Arpeggio #'stencil = #ly:arpeggio::brew-chord-bracket

    \override PianoStaff.InstrumentName #'font-size = #6
    \override PianoStaff.InstrumentName #'font-shape = #'italic
    \override PianoStaff.InstrumentName #'font-magnification = #3
    
    \set PianoStaff.instrumentName = "  2."
    \new Staff =  "up" {
      \override Staff.DynamicLineSpanner   #'direction = #DOWN
      \clef G <<\global \new Voice =  "upv" \righta >>
    }
    \new Staff =  "mid" {
    \override Staff.InstrumentName #'font-size = #0
    \override Staff.InstrumentName #'font-shape = #'upright
    \override Staff.InstrumentName #'font-magnification = #1
    \override Staff.InstrumentName #'extra-offset = #'(0 . 6)
   % \set Staff.instrumentName = "\\begin{turn}{-90}{Rechte Hand}\\end{turn}"
    \set Staff.instrumentName = \markup { \column { Rechte Hand }  \hspace #2 }
      \clef F <<\global \new Voice =  "midv" \rightb>>
    }
      \new Staff =  "down" {
        \override Staff.DynamicLineSpanner   #'direction = #UP
        \clef F
	<< \global \new Voice \lefta \new Voice \leftb >>
    }
  >>
  \layout { 
    \context {
      \RemoveEmptyStaffContext
    }
    \context {
      \Score
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 8)
    }
  }
  
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 100 8)
      }
    }


}


