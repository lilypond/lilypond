\version "2.27.6"

\header {
  texidoc = "Tempo spanner is started by @code{\\tempoSpan} and left-bounded
by @code{TempoMark}, @code{MetronomeMark} or @code{NonMusicalPaperColumn}.
It is ended by the next @code{MetronomeMark}, @code{TempoMark} or the last
@code{NonMusicalPaperColumn} in score.  Consecutive spanners work nicely."
}

#(ly:set-option 'warning-as-error #t)

{
  \tempo 4=120
  \tempoSpan
  b2 b
  \tempoMark "accel."
  \tempoSpan
  b2 b
  \tempo "Vivace"
  b b\tempoMark "rall."

  b b
  \tempoSpan
  b b
  \tempo 4=120
  b b
  \tempoMark "stringendo"
  \tempoSpan
  b b
  \break
  b b
  \tempo "Allegro molto"
  b b
  \override Score.TempoSpanner.to-barline = ##f
  \tempoMark "poco"
  \tempoSpan
  b
  \tempoMark "a"
  \tempoSpan
  b
  \break
  \tempoMark "poco"
  \tempoSpan
  b
  \tempoMark "rallentando"
  \tempoSpan
  b
  b1
  \fine
}
