\version "2.25.32"

\header {
  texidoc = "@code{\\time} accepts an option specifying measure subdivision.  A
dotted bar line should appear between beat groups as noted above each staff."
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
    \submeasureBarsOn
  }
  \context {
    \Staff
    \remove Clef_engraver
    \override TimeSignature.break-visibility = #end-of-line-invisible
  }
}

\fixed c' {
  << {
    \tempo "default: no submeasure bar line"
    \time 5/4
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1 1 1)
    \contextPropertyCheck Timing.measureLength #5/4
    \contextPropertyCheck Timing.submeasureStructure #'(5)
    \contextPropertyCheck Timing.timeSignature 5/4
    \repeat unfold 20 d'16
  } \\ {
    \repeat unfold 5 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>

  \break

  << {
    \tempo "2, 1 | 2"
    \time #'((2 1) (2)) 5/4
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(2 1 2)
    \contextPropertyCheck Timing.measureLength #5/4
    \contextPropertyCheck Timing.submeasureStructure #'(3 2)
    \contextPropertyCheck Timing.timeSignature 5/4
    \repeat unfold 20 d'16
  } \\ {
    \repeat unfold 5 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>

  \break

  << {
    \tempo "2 | 2, 1"
    \time #'((2) (2 1)) 5/4
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(2 2 1)
    \contextPropertyCheck Timing.measureLength #5/4
    \contextPropertyCheck Timing.submeasureStructure #'(2 3)
    \contextPropertyCheck Timing.timeSignature 5/4
    \repeat unfold 20 d'16
  } \\ {
    \repeat unfold 5 {
      \tuplet 3/2 \repeat unfold 6 g16
    }
  } >>
}
