\version "2.25.22"

\header {
  lsrtags = "rhythms"

  texidoc = "
Context properties control the grouping of beats within a measure:
@code{beatStructure} lists the length of each beat in units of
@code{beatBase}.  Default values are established in
@code{scm/time-signature-settings.scm}.  These properties may be
changed particularly with @code{\\set}.

Alternatively, @code{\\time} optionally accepts a beat structure to use
instead of the default.  @code{\\time} applies to the @code{Timing}
context, so it does not reset values of properties that are set in
lower-level contexts such as @code{Voice}.

If the @code{Measure_grouping_engraver} is included in one of the
display contexts, measure grouping signs will be created.  Such signs
ease reading rhythmically complex modern music. In the example, the 9/8
measure is grouped in two different patterns using the two different
methods, while the 5/8 measure is grouped according to the default
setting in @code{scm/time-signature-settings.scm}.  For the 4/4 measure
you have to explicitly set @code{beatBase} to eighths so that the bar's
irregular pattern gets displayed.
"

  doctitle = "Conducting signs, measure grouping signs"
}


\score {
  \new Voice \relative c'' {
    \time 9/8
    g8 g d d g g a( bes g) |
    \set Timing.beatStructure = 2,2,2,3
    g8 g d d g g a( bes g) |
    \time 4,5 9/8
    g8 g d d g g a( bes g) |
    \time 5/8
    a4. g4 |
    \time 3,3,2 4/4
    \set Timing.beatBase = #1/8
    f4 d8 f4 d8 g4
  }
  \layout {
    \context {
      \Staff
      \consists "Measure_grouping_engraver"
    }
  }
}
