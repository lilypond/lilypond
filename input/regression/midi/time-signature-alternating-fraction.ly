\version "2.25.28"

\header {
  texidoc = "The MIDI time signature derived from an alternating time signature
uses the largest denominator of the component time signatures.  In this case,
the expected time signature is 10/8, and the expected metronome beat is 1/8.
Verifying the MIDI output requires manual inspection."
}

%% Of course, it would be better if Time_signature_performer could decompose the
%% alternating time signature and insert changes for each component; however,
%% given the current limitations, 10/8 is better than 5/4.

#(ly:set-option 'warning-as-error #t)

\score {
  \fixed c' {
    \compoundMeter #'((2 4) (6 8))
    \contextPropertyCheck Timing.beamExceptions #'()
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(2 2  3 3)
    \contextPropertyCheck Timing.measureLength #5/4
    \contextPropertyCheck Timing.timeSignature #'((2 . 4) (6 . 8))

    c2 d2.
  }

  \midi {
  }
}
