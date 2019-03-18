\version "2.21.0"

\include "swing.ly"

\header {
  texidoc = "Tests for swing.ly"
}

\score {
  <<
    \new Staff \with {
      instrumentName = \markup \center-column { "notes with" "applied swing" }
    }{
      <>^\markup \column { "swung eigths"
                           "\\tripletFeel 8" }
      \tripletFeel 8 { c'8 c' c' c'
                       c' c' c' c' }
      <>^\markup \column {
        "smoother swung eigths"
        "\\applySwing 8 #'(3 2)" }
      \applySwing 8 #'(3 2) { c'8 c' c' c'
                              c' c' c' c' }
      <>^\markup \column { "swung sixteenths"
                           "\\tripletFeel 16" }
      \tripletFeel 16 { c'16 c' c' c'
                        c' c' c' c'
                        c' c' c' c'
                        c' c' c' c' }
      <>^\markup \column { "straight fourths read as dotted"
                           "\\applySwing 4 #'(3 1)" }
      \applySwing 4 #'(3 1) { c'4 c' c' c' }
      <>^\markup \column { "samba swing"
                           "\\applySwing 16 #'(3 2 2 3)" }
      \applySwing 16 #'(3 2 2 3) { c'16 c' c' c'
                                   c' c' c' c'
                                   c' c' c' c'
                                   c' c' c' c' }
      r8
      <>^\markup \column { "smoother samba swing, start off-beat"
                           "\\applySwingWithOffset 16 #'(4 3 3 4)"
                           "    #(ly:make-moment 1/8)" }
      \applySwingWithOffset 16 #'(4 3 3 4) #(ly:make-moment 1/8) {
        c'16 c'
        c' c' c' c'
        c' c' c' c'
        c' c' c' c'
      }
    }
    \new Staff \with {
      instrumentName = \markup \center-column { "corresponding" "to" }
    }{
      \repeat unfold 4 { \tuplet 3/2 { c'4 c'8 } }
      \repeat unfold 4 { \tuplet 5/4 { c'8. c'8 } }
      \repeat unfold 8 { \tuplet 3/2 { c'8 c'16 } }
      \repeat unfold 2 { c'4. c'8 }
      \repeat unfold 4 { \tuplet 5/4 { c'16. c'16 c' c'16. } }
      r8 \tuplet 7/8 { c'32. c'16}
         \repeat unfold 3 { \tuplet 7/8 { c'16 c'32. c' c'16 } }
    }
  >>
  \header { piece = "1. Swing type demos" }
  \layout { indent = 3\cm }
  \midi {}
}


tripletFeelTestMusic = \relative es' {
  <>^\markup \column { "quarter notes" "should remain unaffected" }
  g4 g c e,  |

  <>^\markup \column {
    "syncopation without tie"
    "grace note should not create confusion"
  }
  \acciaccatura f8
  g4 g b8 d,4 g8 |

  <>^\markup { "syncopation with ties" }
  e4 e8 f8 ~ f a ~ a g8~ |
  g4

  <>^\markup { "music with chords" }
    <c, e>4 <e g>8 <g b>8 <e g c>8 b'8 |

  <>^\markup { "parallel music" }
  <<
    { g8 g g a  <a b> <a c>4 g8 ~ |
      g8 g a b g g a f
    } \\
    { e4 e8 f4 r e8 ~ |
      e4 e8 r e e d4
    }
  >>

  <>^\markup { "with notes shorter than swingDiv" }
  {
    g16 g g g
    g16 g g8
    g8 g16 g
    g8 g16 g32 g |
  }
}

\score {
  <<
    \new Staff \with {
      instrumentName = "straight"
      \remove "Staff_performer"
    } {
      \new Voice \with { \remove "Text_engraver" } \tripletFeelTestMusic
    }
    \new Staff \with {
      instrumentName = "\\tripletFeel 8"
    } {
      \tripletFeel 8 \tripletFeelTestMusic
    }
    \new Staff \with {
      instrumentName = \markup \center-column { "triplet pulse"
                                                "for comparison"}
      \remove "Staff_performer"
    } {
      \repeat unfold 7 { \repeat unfold 4 { \tuplet 3/2 { c''8 c'' c'' } }}
    }
  >>
  \header { piece = "2. Triplet feel in various situations"}
  \layout { indent = 3\cm }
  \midi {}
}

