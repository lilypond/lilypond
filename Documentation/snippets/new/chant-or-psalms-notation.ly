\version "2.17.6"

\header {
  lsrtags = "rhythms, vocal-music, ancient-notation, contexts-and-engravers, specific-notation"

  texidoc = "
This form of notation is used for the chant of the Psalms, where verses
aren't always the same length.

"
  doctitle = "Chant or psalms notation"
}

stemOff = \hide Staff.Stem
stemOn  = \undo \stemOff

\score {
  \new Staff \with { \remove "Time_signature_engraver" }
  {
    \key g \minor
    \cadenzaOn
    \stemOff a'\breve bes'4 g'4
    \stemOn a'2 \bar "||"
    \stemOff a'\breve g'4 a'4
    \stemOn f'2 \bar "||"
    \stemOff a'\breve^\markup { \italic flexe }
    \stemOn g'2 \bar "||"
  }
}

