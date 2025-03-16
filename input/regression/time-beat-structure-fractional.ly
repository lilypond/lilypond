\version "2.25.25"

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

workaround = { % TODO: Support \time #'(5/2 . 4)
  \once \override Staff.TimeSignature.fraction = #'(5/2 . 4)
  \set Timing.beatBase = #1/4
}

\fixed c' {
  << {
    \tempo "placeholder for default"
    \time #'(1 1 1/2) 5/8
    \workaround
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1/2)
    \contextPropertyCheck Timing.measureLength #5/8
    \contextPropertyCheck Timing.timeSignatureFraction #'(5 . 8)
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
    \time #'(1 1 1/2) 5/8
    \workaround
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1/2)
    \contextPropertyCheck Timing.measureLength #5/8
    \contextPropertyCheck Timing.timeSignatureFraction #'(5 . 8)
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
    \time #'(1/2 1 1) 5/8
    \workaround
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1/2 1 1)
    \contextPropertyCheck Timing.measureLength #5/8
    \contextPropertyCheck Timing.timeSignatureFraction #'(5 . 8)
    \repeat unfold 10 d'16
  } \\ {
    \tuplet 3/2 \repeat unfold 3 g16
    \repeat unfold 2 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>
}
