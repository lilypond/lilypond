\version "2.27.3"

\header {
  texidoc = "A metronome beat longer than (approximately) a breve triggers a
warning.  The MIDI output receives a shorter value than the ideal.  Verifying
the MIDI output requires manual inspection."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "unsupported MIDI metronome beat: %s")
   "3")

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "unsupported MIDI metronome beat: %s")
   "6")

%% See 24/1 IRL: "Brobdingnagische Gigue" by Telemann.
\score {
  \fixed c' {
    %% Set a tempo that makes playback for testing more convenient.  This is
    %% still a bit slow, but at 720, MuseScore 4 ignores the metronome
    %% specified in the MIDI file and performs every third click.
    \tempo 4 = 719
    %% in MIDI file: time signature 24/1, metronome 1 (96 clocks).  (metronome
    %% 3 would reflect the beat structure better, but is too large for MIDI)
    \time 24/1
    \contextPropertyCheck Timing.beatBase 1
    \contextPropertyCheck Timing.beatStructure #'(3 3 3 3 3 3 3 3)
    \*8 c\breve.
    %% in MIDI file: time signature 24/1, metronome 1 (96 clocks).  (metronome
    %% 6 would reflect the beat structure better, but is too large for MIDI)
    \time 6,6,6,6 24/1
    \*4 c\longa.
  }

  \midi {}
}
