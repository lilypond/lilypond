\version "2.27.0"

\header {
  texidoc = "When a gradual tempo change ends with both
@code{\\stopGradualTempoChange} and @code{\\tempo},
@code{\\stopGradualTempoChange} defines the target of the gradual change and
@code{\\tempo} defines the tempo going forward.

In this case, the tempo increases from 72 qpm to 144 qpm, then jumps suddenly
to 240 qpm."
}

#(ly:set-option 'warning-as-error #t)

\score {
  {
    \tempo 4 = 72
    \startGradualTempoChange \default
    \*8 c4
    %% accel. ends at 144 qpm; tempo jumps to 240 qpm
    %% (these commands are intentionally reversed)
    \tempo 4 = 240
    \stopGradualTempoChange 4 144
    \*8 c4
  }
  \midi { }
}
