\version "2.19.80"

\header {
  texidoc = "@code{\\context} finds the current context by type and ID
even when there are matching contexts both above and below.
@verbatim
    StaffGroup A
       \
      StaffGroup A (from here, find StaffGroup A)
         \
        StaffGroup A
@end verbatim
INNER and RESULT should appear in the left margin."
}

\new StaffGroup = "A" <<
  \new StaffGroup = "A" \with { instrumentName = "SOUGHT" } <<
    \new StaffGroup = "A" \with { instrumentName = "INNER" } <<
      s1
    >>
    \context StaffGroup = "A" <<
      %% This name is expected to replace SOUGHT.
      \set StaffGroup.instrumentName = "RESULT"
      s1
    >>
  >>
>>
