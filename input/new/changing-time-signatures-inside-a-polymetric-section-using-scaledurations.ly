\version "2.11.61"
\header {
  lsrtags = "rhythms,contexts-and-engravers"
  texidoc = "The @code{measureLength} property, together with
@code{measurePosition}, determines when a bar line is needed.  However,
when using @code{\\scaleDurations}, the scaling of durations makes it
difficult to change time signatures.  In this case, @code{measureLength}
should be set manually, using the @code{ly:make-moment} callback.  The
second argument must be the same as the second argument of
@code{\\scaleDurations}."
  doctitle = "Changing time signatures inside a polymetric section using @code{\\scaleDurations}"
}

\layout {
  \context {
    \Score
    \remove "Timing_translator"
    \remove "Default_bar_line_engraver"
  }
  \context {
    \Staff
    \consists "Timing_translator"
    \consists "Default_bar_line_engraver"
  }
}

<<
  \new Staff {
    \scaleDurations #'(8 . 5) {
      \time 6/8
      \set Timing.measureLength = #(ly:make-moment 3 5)
      b8 b b b b b
      \time 2/4
      \set Timing.measureLength = #(ly:make-moment 2 5)
      b4 b
    }
  }
  \new Staff {
    \clef bass
    \time 2/4
    c2 d e f
  }
>>
