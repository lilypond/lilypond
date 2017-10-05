\version "2.21.0"
%\include "example-header.ily"


\header {
 mutopiatitle = "Jesu, meine Freude"
 mutopiacomposer = "BachJS"
 poet = "BWV 610"
 %mutopiaopus = "BWV 610"
 mutopiainstrument = "Organ"
 date = ""
 source = "Bach-Album, Ernst H. Wolfram (editor), 6787, C. F. Peters, Leipzig"
 style = "Baroque"
 copyright = "Public Domain"
 maintainer = "Urs Metzger"
 maintainerEmail = "urs@ursmetzger.de"
 lastupdated = "2006/Mar/15"

 title = "Jesu, meine Freude"
 composer = "Johann Sebastian Bach"
 %opus="BWV 610"

 footer = "Mutopia-2006/03/27-706"
 tagline = \markup {
  \override #'(baseline-skip . 2.2)
  \center-column {
   \abs-fontsize #8 \sans \bold
   \with-url "http://www.MutopiaProject.org" {
    "Mutopia"
    "Project"
   }
  }
  \override #'(baseline-skip . 0)
  \column {
   \with-color #grey
   \filled-box #'( 0 . 1) #'(-2.5 . 1.4) #0
  }
  \override #'(baseline-skip . 2.2)
  \column {
   \abs-fontsize #8 \sans
   \concat{"Typeset using " \with-url "http://www.lilypond.org" "LilyPond" " ©2006 by " \maintainer "." }
   \abs-fontsize #8 \sans
   \concat {"This work is licensed under a " \with-url "http://creativecommons.org/about/pdm" \copyright " license."}
  }
 }
}

\include "deutsch.ly"

global = {
   \key c \minor
   \time 4/4
   \tempo Largo
   \accidentalStyle default
}

halsup = {
  \voiceOne
}

halsdown = {
  \voiceTwo
}

staffup = {
   \change Staff = "right" \halsdown
}

staffdown = {
   \change Staff = "left" \halsup
}

sopran = {
   \new Voice \relative g' {
      \global
      \halsup
      \repeat volta 2 {
         g4 g f es
         d2 c\fermata
         g'4 g a h
         c2 h\fermata
         %% Takt 5 ==============================================
         c8. d16 es4 d4. d8
         c1\fermata
      }
      g4 g as g
      f4. f8 es2\fermata
      g4 g a h
      %% Takt 10 =============================================
      c4 b! a2
      g1\fermata
      g4 g f es
      d2 c2\fermata \bar "|."
   }
}

alt = {
   \new Voice \relative es' {
      \global
      \halsdown
      \repeat volta 2 {
         es16 d es8~ 16 f es d c8 d~ d c
         c8 c4 h8 c8. \staffdown g16 \staffup c h c d
         es16 d es8~ 16 f es d c8 d16 es f as g f~
         16 e f8~ 16 g f es d es d8~ 16 es f d
         %% Takt 5 ==============================================
         g8 f es16 g as es f es f d g as g f
         e8 f16 g as g as e f g f8~ 16 f es d
      }
      es16 d es8~ 16 f es des c8 \staffdown b~ 16 b c g
      as16 g as f b c b as g f g8~ 16 b \staffup c d
      es16 d es8~ 16 f es d c8 d16 es f as g f
      %% Takt 10 =============================================
      es16 d es8 d g~g g4 fis8
      g16 d es c \staffdown d h c a h a h c d h g h
      c16 h c \staffup d es d es8~ 8 d4 c8~
      16 h c8~ 8 h c16 \staffdown g as8 g4\fermata
   }
}

tenor = {
   \new Voice \relative {
      \global
      \repeat volta 2 {
         c'16 h c8~ 16 h c g a8 g~ 16 g as es
         f16 es f d g as g f es d \tieDown es8~ 16 \tieNeutral f es d
         c16 h c g' c h c c, f8. g16 as!4~
         16 g as b c h c8 d8. c16 h c d h
         %% Takt 5 ==============================================
         es16 d es d~ 8 c~c c4 h8
         c4~ 8. b16 as b as8 g16 as g f
      }
      \halsdown es16 f g as b des c b as g f8~ 8 es
      es8 es4 d8 es8. b16 es d es f
      es16 f g8 c16 d c b a g f8~ 4
      %% Takt 10 =============================================
      g16 fis g a b a b g c b c a d es d c
      h8 c16 a h g a fis g8 d16 es f es f8~
      16 d es h c h c8 r16 f g d es g as fis
      g16 d f! es f as g f e8. f16~ 16 d e8\fermata
   }
}

right = {
   \clef treble
   <<
   \alt
   \sopran
   >>
}

left = {
   \clef bass
   <<
   \tenor
   >>
}

pedal = {
   \global
   \clef "bass"
   \relative {
      \repeat volta 2 {
         r8 c16 d es d es8~ 16 a, h g c h c8
         r16 g as f g f g8 c,2
         r8 c'16 d es d es8~ 16 c f es d c d8
         c8 f16 g as g as8~ 16 d, fis d g fis g8
         %% Takt 5 ==============================================
         r8 a16 h c h c8
         r16 g as f g f g8
         r16 g as e f e f8 r16 e f h, c h c8
      }
      r8 es16  f g f g8~ 16 c, d! b es d es8
      r16 b c as b as b8 es,2
      r8 c'16 d es d es8~ 16 c f es d c d8
      %% Takt 10 =============================================
      c8 es16 f! g fis g8 r16 d es c d c d8
      g,1
      c4 c,8 c'16 b! a8 h c16 h c8
      r16 g as fis g fis g8 c,2_\fermata
   }
}

\score {
   \new PianoStaff
      { \set PianoStaff.instrumentName = \markup { \large \center-column {
          "a" "" "2 Clav." "" "e"  ""  "Pedale." } \hspace #0.5 }
      <<
         \context Staff = right
         {
            \context Voice = right \right
         }
         \context Staff = left {
            \context Voice = left \left
         }
         \context Staff = pedal {
            \context Voice = pedal \pedal
         }
      >>
   }
   \layout{}
}
