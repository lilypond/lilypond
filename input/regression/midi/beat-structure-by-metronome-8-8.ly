\version "2.25.35"

\header {
  texidoc = "A uniform compound beat structure is reflected in the MIDI
metronome.  Verifying the MIDI output requires manual inspection."
}

#(ly:set-option 'warning-as-error #f)

\midi {
  \context {
    \Score
    %% Timing_translator complains if timeSignatureSettings is empty.
    timeSignatureSettings = #'((dummy . dummy))
  }
}

\score {
  \fixed c' {
    %% in MIDI file: time signature 8/8, metronome 1/8
    \time 8/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(1 1 1 1 1 1 1 1)
    \*8 c8

    %% in MIDI file: time signature 8/8, metronome 1/4
    \time 2,2,2,2 8/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(2 2 2 2)
    \*8 d8

    %% in MIDI file: time signature 8/8, metronome 1/2
    \time #'(4 4) 8/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(4 4)
    \*8 e8

    %% in MIDI file: time signature 8/8, metronome 1
    \time #'(8) 8/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(8)
    \*8 f8
  }

  \midi {}
}
