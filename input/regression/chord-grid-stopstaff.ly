\version "2.23.10"

\header {
  texidoc = "@code{\\stopStaff} and @code{\\startStaff} can be used
in chord grids."
}

\paper {
  indent = 0
}

#(ly:set-option 'warning-as-error)

#(ly:expect-warning "removing chord names on chord square because no staff symbol was found")
#(ly:expect-warning "system with empty extent")

\new ChordGrid \chordmode {
  c1
  c1
  \stopStaff
  s1
  \startStaff
  c1
  \break
  c1
  \stopStaff
  s1*2
  \startStaff
  c1
  \break
  c1
  \stopStaff
  \override StaffSymbol.line-positions = #'(-16 16)
  \startStaff
  c1
  c1
  c1
  %% Chords in \stopStaff should be handled gracefully.
  \break
  \stopStaff
  c1
}
