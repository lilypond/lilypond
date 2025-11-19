\version "2.25.23"

\header {
  categories = "Contemporary notation, Contexts and engravers, Rhythms,
                Workaround"

  texidoc = "
The @code{measureLength} property, together with
@code{measurePosition}, determines when a bar line is needed.  However,
when using @code{\\scaleDurations}, the scaling of durations makes it
difficult to change time signatures.  In this case,
@code{measureLength} should be set manually, using the
@code{ly:make-moment} callback.  The second argument must be the same
as the second argument of @code{\\scaleDurations}.
"

  doctitle = "Changing time signatures inside a polymetric section using \\scaleDurations"
}


\layout {
  \context {
    \Score
    \remove "Timing_translator"
  }
  \context {
    \Staff
    \consists "Timing_translator"
  }
}

<<
  \new Staff {
    \scaleDurations 8/5 {
      \time 6/8
      \set Timing.measureLength = #6/5
      b8 b b b b b
      \time 2/4
      \set Timing.measureLength = #4/5
      b4 b
    }
  }
  \new Staff {
    \clef bass
    \time 2/4
    c2 d e f
  }
>>
