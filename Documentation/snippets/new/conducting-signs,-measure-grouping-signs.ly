\version "2.13.4"

\header {
  lsrtags = "rhythms"
  texidoc = "
Beat grouping within a bar is controlled by the default grouping
established in @code{beamSettings}.  This grouping can be established
by the use of @code{\\overrideBeamSettings}.  Alternatively, the
Scheme function @code{set-time-signature} can be used to both
set the time signature and establish the default grouping rule.
@code{set-time-signature}, takes three arguments: the
number of beats, the beat length, and the internal grouping of beats in
the measure.  If the @code{Measure_grouping_engraver} is included
in one of the display contexts, measure grouping signs will be
created.  Such signs ease reading rhythmically complex modern music.
In the example, the 9/8 measure is grouped in two different
patterns using the two different methods, while the 5/8 measure
is grouped according to the default setting in
@file{scm/beam-settings.scm}:
"
  doctitle = "Conducting signs, measure grouping signs"
}

\score {
  \relative c'' {
    \time 9/8
    \overrideBeamSettings #'Score #'(9 . 8) #'end #'((* . (2 2 2 3)))
    g8 g d d g g a( bes g) |
    #(set-time-signature 9 8 '(4 5))
    g8 g d d g g a( bes g) |
    \time 5/8
    a4. g4 |
  }
  \layout {
    \context {
      \Staff
      \consists "Measure_grouping_engraver"
    }
  }
}
