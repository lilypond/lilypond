\version "2.25.28"

\header {
  texidoc = "@code{\\time} accepts an option specifying the beat structure.  In
this case, it is applied with fractional beats, which is unusual."
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
    \time #'(5/2 . 4)
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1/2)
    \contextPropertyCheck Timing.measureLength #5/8
    \contextPropertyCheck Timing.timeSignature #'(5/2 . 4)
    \repeat unfold 10 d'16
  } \\ {
    \repeat unfold 2 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
    \tuplet 3/2 \repeat unfold 3 g16
  } >>

  \break

  << {
    \tempo "(1 1 ½)"
    \time #'(1 1 1/2) #'(5/2 . 4)
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1/2)
    \contextPropertyCheck Timing.measureLength #5/8
    \contextPropertyCheck Timing.timeSignature #'(5/2 . 4)
    \repeat unfold 10 d'16
  } \\ {
    \repeat unfold 2 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
    \tuplet 3/2 \repeat unfold 3 g16
  } >>

  \break

  << {
    \tempo "(½ 1 1)"
    \time #'(1/2 1 1) #'(5/2 . 4)
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1/2 1 1)
    \contextPropertyCheck Timing.measureLength #5/8
    \contextPropertyCheck Timing.timeSignature #'(5/2 . 4)
    \repeat unfold 10 d'16
  } \\ {
    \tuplet 3/2 \repeat unfold 3 g16
    \repeat unfold 2 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>

  \break

  %% try a fraction other than 1/2 to cover ceiling/floor calculations
  << {
    \time #'(8/3 . 4)
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 2/3)
    d'4 d'4 \tuplet 3/2 d'4
  } \\ {
    g4 g4 \tuplet 3/2 g4
  } >>
}
