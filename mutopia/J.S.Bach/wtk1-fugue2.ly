\header {
  title =       "Fuga a 3 voci"
  opus =        "BWV 847-Fuga"
  source =      "Henle's Urtext"
  composer =    "Johann Sebastian Bach (1685-1750)"
  enteredby =   "hwn, wl, jcn"

  % mutopia headers.
  mutopiatitle = "Das Wohltemperierte Clavier I, Fuga II (c-minor)"
  mutopiacomposer = "J. S. Bach (1685-1750)"
  mutopiainstrument = "Piano"
  mutopiaopus = "BWV847"
  style = "baroque"
  copyright = "Public Domain"
  maintainer = "hanwen@cs.uu.nl"
  mutopiapublicdomain = "\\parbox{\\hsize}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it.}"
  tagline = \mutopiapublicdomain
  lastupdated = "2000/Feb/14"
}


% It would be nice to squeeze this onto two pages, but I am not sure
% if it would work with US paper.

\version "1.3.146"

  
dux =  \context Voice=two \notes \relative c''{
  \voiceTwo
  \clef violin

  r8 c16 b c8 g as c16 b c8 d |
  g, c16 b c8 d f,16 g as4 g16 f |
  es c' b a g f! es d c8 es' d c |
  bes a bes c fis, g a fis |
%%5
  g4 r16 c, d es f g as8~ as16 d, es f |
  g a bes8 ~ bes16 es, f g as g f es d8 c'16 b! |
          % forced accident!
  c4 r4 r8 f es d |
  r8 as g f g f16 es f8 d | 
  g4 r8 b c c16 b c8 g |
%%10 
  as4 r8 a bes bes16 a bes8 f |
  g4 r8 g as as g f |
  r8 
  \translator Staff = bass \stemUp 
  as, bes c r8 as16 g as8 f8 |
  bes8 c bes as bes g f es |
  f des' c bes c as g f |
%%15
  g8
  g'16 fis g8 c, 
  es \translator Staff = treble
  \stemBoth
  \stemDown
  g16 fis! g8 a |
  d, g16 fis g8 a! c,16 d es4 d16 c |  % forced accident!
  bes8 r8 r16 d e fis g a bes8 ~ bes16 e, f g |
  a bes c8 ~ c16 fis,16 g a bes8 es,!16 d es8 g, |
  as f'16 es f8 a,8 bes g'16 f g8 b, |
%%20
  c16 f \translator Staff = bass
  \stemBoth \stemUp
     es d c bes! as g 
  f8 \translator Staff = treble
  \stemBoth \stemDown
  as' g f |
  es d es f b, c d b |
  c4 r8 e8 f f16 e f8 c |
  d4 r8 d8 es8 es16 d es8 bes |
  c2 ~ c8 d16 es f es f d |
%%25
  b8 r8 r b c r r es |
  d r r f ~ f r r f |
  es as g f es d es f |
  b, c d b b c r c |
  f16 d es c ~ c8 b c4 r8 e |
%%30
  f4 r8 f f es16 d es8 <f as> |
  <b, d> r <b d> r <g2 c> |
}


comes =  \context Voice=one \notes \relative c'' {
  \voiceOne
  R1 |
  R1 |
  r8 g'16 fis g8 c, es g16 fis g8 a |
  d,8 g16 fis g8 a c,16 d es4 d16 c |
%%5
  bes8 es16 d es8 g,8 as f'16 es f8 a, 
  bes8 g'16 f g8 b, c8 d16 es f4 ~ |
  f8 es16 d c16 bes ! as g f8 as' g f 
  es d es f b, c d b |
  c g'16 fis g8 d es4 r8 e8 |
%%10 
  f f16 e f8 c8 d4 r8 d |
  es8 es16 d es8 bes c es16 d es8 f |
  bes, es16 d es8 f as,16 bes c4 bes16 as |
  \stemBoth g16 es f g as bes c d es d c d es f g a |
  bes f, g as bes c d e f es d es f g a b |
%%15
  \stemUp c8 b16 a g f! es d c8 es d c |
  bes a bes c fis,! g a fis |    % forced accident
  g8 d'16 c d8 r8 r8 e16 d e8 r8 |
  r fis16 e fis8 r r g,16 f g8 r8 |
  r8 a16 g a8 r r b16 a b8 r |
%%20
  r8 c16 b c8 g as c16 b c8 d |
  g, c16 b c8 d f,16 g as4 g16 f |
  es8 c'16 b c8 g as4 r8 a |
  bes8 bes16 a bes8 f8 g4 r8 g ~ |
  g as16 bes c b c as f2 ~ |
%%25
  f8 d'16 c d8 f, es es'16 d es8 g, |
  f f'16 es f8 as, g16 f' es d c b a g |
  c8 f es d r as g f |
  g f16 es f8 d as' g r a |
  b c f,16 es d c c8 c'16 b c8 g |
%%30
  as c16 b c8 <d b ! as !> g,8 c16 b c8 d |
  f,16 g as4 g16 f e2 |
}

bassdux =  \context Voice=three \notes \relative c' {
  \clef bass
  R1 |
  R |
  R |
  R |
%%5
  R |
  R1 |
  r8 c16 b c8 g as c16 b c8 d |
  g, c16 b c8 d f,16 g as4 g16 f | 
  es c' b a g f es d c d es d c bes! as! g |
          % -> \classic_accidentals
%%10
  f bes' as g f es d c bes c d c bes as g f |
  es as' g f es des c bes as8 c' bes as |
  g8 f g as d, es f d |
  es as g f g es d c |
  d bes' as g as f es d! |
%%15
  es8 r8 r4 r8 c bes a |
  r es' d c d c16 bes c8 d |
  g,8 bes'16 a bes8 d, es c'16 bes c8 e, |
  f d'16 c d8 fis, g4 r16 g, a b |
  c16 d es8~ es16 a, bes c d es f8~ f16 b, c d |
%%20 
% es8 r r e f f, es! d |     % -> \classic_accidentals
  es8 r r e \stemBoth \stemDown f f, es! d \stemBoth |
  r as' g f g f16 es f8 g |
  c16 d es d c bes as g f bes' as g f es d c |
  bes c d c bes as g f es as' g f es d c bes |
  as bes c bes as g f es d g' f es d c b a |
%%25
  g4 r4 r16 g a b c d es f |
  g f as g f es d c b8 c16 b c8 g |
  as c16 b c8 d g, c16 b c8 d |
  f,16 g as4 g16 f es4 r8 es' |
  d c g' g, 
%%30
  \context Staff=bass <
    { \voiceOne c2 ~ | c1 ~ | c1 }
    \context Voice=four { \voiceTwo  c,2 ~ | c1 ~ | c1 }
  >
}

\score {
 
    \notes \context PianoStaff < 
        \property Score.TimeSignature \override #'style = #'C
	\context Staff = treble <
	    \key c \minor
%	\key es \major
	    \dux
	    { \comes \bar "|." }
	      \time 4/4
	  >
	\context Staff = bass <
	    \key c \minor
	    \bassdux
	>
    >

    \paper {
        linewidth = 18.0 \cm
    }
    \header{
	opus = "BWV 847"
    }

    \midi {
	\tempo 4 =84
    }
}

