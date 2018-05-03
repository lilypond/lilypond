\version "2.19.80"

\header {
  texidoc = "@code{\\context} finds a child by type and ID even when
the parent also matches.
@verbatim
    StaffGroup A
       \
      StaffGroup B (from here, find StaffGroup A)
         \
        StaffGroup A (this is found)
@end verbatim
RESULT should appear in the left margin."
}

\new StaffGroup = "A" <<
  \new StaffGroup = "B" <<
    \new StaffGroup = "A" \with { instrumentName = "SOUGHT" } <<
      \context Staff = "a" { s1 }
    >>
    \context StaffGroup = "A" <<
      %% This is expected to replace SOUGHT.
      \set StaffGroup.instrumentName = "RESULT"
      \context Staff = "a" { s1 }
    >>
  >>
>>
