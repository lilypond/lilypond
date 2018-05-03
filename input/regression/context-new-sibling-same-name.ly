\version "2.19.80"

\header {
  texidoc = "@code{\\new} can create a sibling of an existing context
with the same type and name.  The instrument name should be PASS."
}

\new StaffGroup = "A" <<
  \context StaffGroup = "B" \with { instrumentName = "PASS" } <<
    s1
  >>
  \new StaffGroup = "B" {
    %% This does not replace PASS because it is a new context, and it
    %% is not engraved because this context contains no music.
    \set StaffGroup.instrumentName = "FAIL"
  }
>>
