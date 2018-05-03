\version "2.19.80"

\header {
  texidoc = "@code{\\context} creates a new context rather than finding
a matching context in another branch.
@verbatim
             StaffGroup A
            /          \
     StaffGroup B  StaffGroup C (from here, find StaffGroup B)
                         \
                       [StaffGroup B] (this is created)
@end verbatim
B1, A, C, and B2 should appear in the left margin."
}

\new StaffGroup = "A" \with { instrumentName = "A" } <<
  \context StaffGroup = "B" \with { instrumentName = "B1" } <<
    s1
  >>
  \new StaffGroup = "C" \with { instrumentName = "C" } <<
    s1
    \context StaffGroup = "B" <<
      %% This is expected NOT to replace B1.
      \set StaffGroup.instrumentName = "B2"
      s1
    >>
  >>
>>
