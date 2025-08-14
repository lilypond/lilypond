\version "2.25.28"

\header {
  texidoc = "@code{\\time} accepts an option specifying the beat structure.  In
this case, it is applied to a time signature with beats conventionally
subdivided into duples."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  ragged-right = ##t
  \context {
    \Score
    \remove Bar_number_engraver
    %% Timing_translator complains if timeSignatureSettings is empty.
    timeSignatureSettings = #'((dummy . dummy))
    subdivideBeams = ##t
  }
  \context {
    \Staff
    \remove Clef_engraver
    \override TimeSignature.break-visibility = #end-of-line-invisible
  }
}

\fixed c' {
  << {
    \tempo "default"
    \time 3/4
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1)
    \contextPropertyCheck Timing.measureLength #3/4
    \contextPropertyCheck Timing.timeSignature 3/4
    \repeat unfold 12 d'16
  } \\ {
    \repeat unfold 3 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>

  \break

  << {
    \tempo "(2 1)"
    \time 2,1 3/4
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(2 1)
    \contextPropertyCheck Timing.measureLength #3/4
    \contextPropertyCheck Timing.timeSignature 3/4
    \repeat unfold 12 d'16
  }\\ {
    \repeat unfold 3 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>

  \break

  << {
    \tempo "(1 2)"
    \time 1,2 3/4
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 2)
    \contextPropertyCheck Timing.measureLength #3/4
    \contextPropertyCheck Timing.timeSignature 3/4
    \repeat unfold 12 d'16
  } \\ {
    \repeat unfold 3 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>
}
