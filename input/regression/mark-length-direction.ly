\version "2.25.21"

\header {
  texidoc = "@code{\\markLengthOn} also works nicely regardless of
@code{direction}."
}

\paper { ragged-right = ##t }

mus = {
  \mark "long RehearsalMark"
  c''2 2
  \tempo "long MetronomeMark"
  c''2 2
  \textMark "long TextMark"
  c''2 2
}

{
  \override Score.RehearsalMark.self-alignment-X = #LEFT
  \markLengthOn
  \mus
  \break
  \override Score.RehearsalMark.direction = #DOWN
  \override Score.MetronomeMark.direction = #DOWN
  \override Score.TextMark.direction = #DOWN
  \mus
}
