\version "2.25.32"

\header {
  texidoc = "@code{\\time} accepts an option specifying the beat structure.  In
this case, it is applied to a time signature with beats conventionally
subdivided into triples."
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
    \time 9/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(3 3 3)
    \contextPropertyCheck Timing.measureLength #9/8
    \contextPropertyCheck Timing.submeasureStructure #'(9)
    \contextPropertyCheck Timing.timeSignature 9/8
    \repeat unfold 18 d'16
  } \\ {
    \repeat unfold 3 {
      \tuplet 2/3 \repeat unfold 4 g16
    }
  } >>

  \break

  << {
    \tempo "(6 3)"
    \time 6,3 9/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(6 3)
    \contextPropertyCheck Timing.measureLength #9/8
    \contextPropertyCheck Timing.submeasureStructure #'(9)
    \contextPropertyCheck Timing.timeSignature 9/8
    \repeat unfold 18 d'16
  } \\ {
    \repeat unfold 3 {
      \tuplet 2/3 \repeat unfold 4 g16
    }
  } >>

  \break

  << {
    \tempo "(3 6)"
    \time 3,6 9/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(3 6)
    \contextPropertyCheck Timing.measureLength #9/8
    \contextPropertyCheck Timing.submeasureStructure #'(9)
    \contextPropertyCheck Timing.timeSignature 9/8
    \repeat unfold 18 d'16
  } \\ {
    \repeat unfold 3 {
      \tuplet 2/3 \repeat unfold 4 g16
    }
  } >>
}
