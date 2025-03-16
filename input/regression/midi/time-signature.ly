\version "2.25.26"

\header {
  texidoc = "The @code{\\time} command changes the time signature and metronome
in MIDI output.  Verifying the MIDI output requires manual inspection."
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
    %% in MIDI file: time signature 5/8, metronome 1/4
    \time #'(5/2 . 4)
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1 1 1/2)
    \repeat unfold 5 c8

    %% in MIDI file: time signature 5/8, metronome 1/8
    \time 5/8
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(1 1 1 1 1)
    \repeat unfold 5 d8

    %% in MIDI file: time signature 3/4, metronome 1/4
    \time 9/12
    \contextPropertyCheck Timing.beatBase #1/12
    \contextPropertyCheck Timing.beatStructure #'(3 3 3)
    \repeat unfold 6 f8

    %% in MIDI file: time signature 6/8, metronome 3/8
    \time #'(2 . 8/3)
    \contextPropertyCheck Timing.beatBase #3/8
    \contextPropertyCheck Timing.beatStructure #'(1 1)
    \repeat unfold 6 g8

    %% in MIDI file: time signature 2/1, metronome 2
    \time #'(1 . 1/2)
    \contextPropertyCheck Timing.beatBase #2
    \contextPropertyCheck Timing.beatStructure #'(1)
    \repeat unfold 16 a8

    %% in MIDI file: time signature 255/32, metronome 85/32 (255 clocks)
    \time #'(3 . 32/85)
    \contextPropertyCheck Timing.beatBase #85/32
    \contextPropertyCheck Timing.beatStructure #'(1 1 1)
    \repeat unfold 3 {
      b32 32
      1 1 2 % TODO: Use \breve~ 2; midi2ly currently gets it wrong
      32 32 32
    }

    %% in MIDI file: time signature 1/32, metronome 1/96 (1 clock)
    \time 3/96
    \contextPropertyCheck Timing.beatBase #1/96
    \contextPropertyCheck Timing.beatStructure #'(1 1 1)
    c'32
  }

  \midi {}
}
