\version "2.17.30"
\header  {
  texidoc = "Feathered beams should have the same progress of their feathering
at the end of a line break as they do at the beginning of the next line."
}

\paper {
  left-margin = 2\cm
  line-width = 10\cm
  ragged-right = ##t
  indent = 0\cm
}

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d e f g a ]
    \once \override Voice.Beam.grow-direction = #LEFT
    a[ g f e d c b a]  \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d \bar "" \break e f g a b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d e f g a \bar "" \break b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d e f g a b c d \bar "" \break e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #LEFT
    a32[ b c d \bar "" \break e f g a b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #LEFT
    a32[ b c d e f g a \bar "" \break b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #LEFT
    a32[ b c d e f g a b c d \bar "" \break e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Stem.direction = #DOWN
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d \bar "" \break e f g a b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Stem.direction = #DOWN
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d e f g a \bar "" \break b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Stem.direction = #DOWN
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #RIGHT
    a32[ b c d e f g a b c d \bar "" \break e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Stem.direction = #DOWN
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #LEFT
    a32[ b c d \bar "" \break e f g a b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Stem.direction = #DOWN
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #LEFT
    a32[ b c d e f g a \bar "" \break b c d e f g a ] \bar "|"
} >>

\new Staff <<
  \relative c' {
    \cadenzaOn
    \omit Staff.TimeSignature
    \override Voice.Stem.direction = #DOWN
    \override Voice.Beam.breakable = ##t
    \once \override Voice.Beam.grow-direction = #LEFT
    a32[ b c d e f g a b c d \bar "" \break e f g a ] \bar "|"
} >>
