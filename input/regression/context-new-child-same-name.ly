\version "2.19.80"

\header {
  texidoc = "@code{\\new} can create a child of the same type and name
as its parent.  PASS should appear in the left margin."
}

\context StaffGroup = "B" \with { instrumentName = "PASS" } <<
  \new StaffGroup = "B" {
    %% This does not replace PASS because it is a new context, and
    %% it is not engraved because this context contains no music.
    \set StaffGroup.instrumentName = "FAIL"
  }
  s1
>>
