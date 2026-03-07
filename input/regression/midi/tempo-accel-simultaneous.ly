\version "2.27.0"

\header {
  texidoc = "@code{\\startGradualTempoChange} and
@code{\\stopGradualTempoChange} may appear in simultaneous contexts with the
same arguments."
}

#(ly:set-option 'warning-as-error #t)

\score {
  <<
    \fixed c' {
      \*4 c4
      \startGradualTempoChange \default
      \*2 \*4 c4
      \stopGradualTempoChange 4 240
      c4
    }
    \fixed c {
      \*8 f8
      \startGradualTempoChange \default
      \*2 \*8 f8
      \stopGradualTempoChange 4 240
      f4
    }
  >>
  \midi { }
}
