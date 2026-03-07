\version "2.27.0"

\header {
  texidoc = "When a gradual tempo change ends with
@code{\\stopGradualTempoChange} only -- without @code{\\tempo} --
@code{\\stopGradualTempoChange} defines the target of the gradual change and
the original tempo is resumed.

In this case, the tempo decreases from 120 qpm to 60 qpm, jumps suddenly
back to 120 qpm, and increases from there to 240 qpm."
}

#(ly:set-option 'warning-as-error #t)

\score {
  {
    \tempo 4 = 120
    \startGradualTempoChange \default
    \*8 c4
    \stopGradualTempoChange 4 60
    %% no \tempo -> "a tempo"
    %% rit. ends at 60 qpm; tempo jumps back to 120 qpm
    \startGradualTempoChange \default
    \contextPropertyCheck Timing.tempoWholesPerMinute #120/4
    \*8 c4
    \tempo 4 = 240
    c1
  }
  \midi { }
}
