\version "2.25.26"

\header {
  texidoc = "The @code{\\time} command changes the time signature and metronome
in MIDI output.  Verifying the MIDI output requires manual inspection."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "Unsupported MIDI time signature: (%s)/(%s)")
  "8/3" "4")

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "Unsupported MIDI time signature: (%s)/(%s)")
  "8" "12")

\midi {
  \context {
    \Score
    %% Timing_translator complains if timeSignatureSettings is empty.
    timeSignatureSettings = #'((dummy . dummy))
  }
}

\score {
  \fixed c' {
    %% in MIDI file: time signature ???, metronome 1/4
    \time #'(8/3 . 4)
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 2/3)
    \tuplet 3/2 { \repeat unfold 8 c8 }
    \tuplet 3/2 { \repeat unfold 8 d8 }
    \tuplet 3/2 { \repeat unfold 8 e8 }

    %% in MIDI file: time signature ???, metronome 1/12
    \time 8/12
    \contextPropertyCheck Timing.beatBase #1/12
    \contextPropertyCheck Timing.beatStructure #'(1 1 1 1 1 1 1 1)
    \tuplet 3/2 { \repeat unfold 8 f8 }
    \tuplet 3/2 { \repeat unfold 8 g8 }
    \tuplet 3/2 { \repeat unfold 8 a8 }
  }

  \midi {}
}
