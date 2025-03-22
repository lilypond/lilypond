\version "2.25.26"

\header {
  texidoc = "A uniform compound beat structure is reflected in the MIDI
metronome.  Verifying the MIDI output requires manual inspection."
}

#(ly:set-option 'warning-as-error #t)

\midi {
  \context {
    \Score
    %% Timing_translator complains if timeSignatureSettings is empty.
    timeSignatureSettings = #'((dummy . dummy))
  }
}

\score {
  \fixed c' {
    %% in MIDI file: time signature 12/8, metronome 1/4
    \time 2,2,2,2,2,2 12/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(2 2 2 2 2 2)
    \repeat unfold 12 c8

    %% in MIDI file: time signature 12/8, metronome 3/8
    \time 12/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(3 3 3 3)
    \repeat unfold 12 d8

    %% in MIDI file: time signature 12/8, metronome 1/2
    \time 4,4,4 12/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(4 4 4)
    \repeat unfold 12 e8

    %% in MIDI file: time signature 12/8, metronome 3/4
    \time 6,6 12/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(6 6)
    \repeat unfold 12 f8

    %% in MIDI file: time signature 12/8, metronome 3/2
    \time #'(12) 12/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(12)
    \repeat unfold 12 g8
  }

  \midi {}
}
