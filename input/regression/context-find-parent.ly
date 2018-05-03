\version "2.19.80"

\header {
  texidoc = "@code{\\context} can find the parent context by type and ID.
@verbatim
    StaffGroup A
       \
      StaffGroup B (from here, find StaffGroup A)
@end verbatim
CHILD and RESULT should appear in the left margin."
}

\new StaffGroup = "A" \with { instrumentName = "SOUGHT" } <<
  \new StaffGroup = "B" \with { instrumentName = "CHILD" } <<
    s1
    \context StaffGroup = "A" <<
      %% This name is expected to replace SOUGHT.
      \set StaffGroup.instrumentName = "RESULT"
      s1
    >>
  >>
>>
