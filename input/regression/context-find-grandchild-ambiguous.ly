\version "2.19.80"

\header {
  texidoc = "@code{\\context} finds a grandchild by type and ID when
there are multiple matching contexts.
@verbatim
           StaffGroup A (from here, find Staff D)
          /            \
    StaffGroup B  StaffGroup C
        /                \
    Staff D            Staff D
@end verbatim
RESULT and either B or C should appear in the left margin."
}

\new StaffGroup = "A" <<
  \new StaffGroup = "B" \with { instrumentName = "B" } <<
    \context Staff = "D" { b'1 }
  >>
  \new StaffGroup = "C" \with { instrumentName = "C" } <<
    \context Staff = "D" { d'1 }
  >>
  \context Staff = "D" <<
    %% This name is expected to replace either B or C.  It would
    %% probably be helpful for LilyPond to warn that there is more
    %% than one matching context in the scope of the search.
    \set StaffGroup.instrumentName = "RESULT"
    s1
  >>
>>
