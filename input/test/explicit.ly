\version "1.7.18"

\header{ texidoc="@cindex Explicit Pitch and Duration
Explicit pitches and durations. "
}
\score {
  \notes {
    \pitch #(ly:make-pitch 0 0 0) \duration #(ly:make-duration 1 0)
    \pitch #(ly:make-pitch 1 1 1) \duration #(ly:make-duration 2 0)

  }
}
%% new-chords-done %%
