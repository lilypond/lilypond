\version "2.27.0"

\header {
  texidoc = "When a gradual tempo change ends with
@code{\\stopGradualTempoChange} only -- without @code{\\tempo} --
@code{\\stopGradualTempoChange} defines the target of the gradual change and
the original tempo is resumed.

In this case, the tempo decreases from 120 qpm to 90 qpm, then jumps suddenly
back to 120 qpm."
}

#(ly:set-option 'warning-as-error #t)

\score {
  {
    \tempo 4 = 120
    \startGradualTempoChange \default
    \*8 c4
    \stopGradualTempoChange 4 90
    %% no \tempo -> "a tempo"
    %% rit. ends at 90 qpm; tempo jumps back to 120 qpm
    \*8 c4
  }
  \midi { }
}
