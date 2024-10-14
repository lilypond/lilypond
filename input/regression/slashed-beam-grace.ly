\version "2.25.21"

\header {
  texidoc = "Printing slashed beams is the default for @code{\slashedGrace}."
}

mus = {
  \slashedGrace { d'16 } c'1
  \slashedGrace { d'16 e' d' } c'1
  \slashedGrace { d16 e'' d' } c'1
  \slashedGrace { d'16 e' d' f' g'' } c'1
  \slashedGrace { d''16 e'' d' f' g } c'1
}

music = { \stemUp \mus \bar "||" \stemDown \mus \bar "|." }

graceTestLayout =
  \layout {
    \context {
      \Voice
      %% needed to test for stem up/down graces
      $(remove-grace-property 'Voice 'Stem 'direction)
    }
  }

\score { \music \layout { \graceTestLayout } }

\score {
  \music
  \layout { \graceTestLayout \override Beam.details.slash-side = #RIGHT }
}
