\version "2.27.4"

\header {
  texidoc = "This is a regression test for issue 6963.  It should not crash."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning "grob does not belong to a VerticalAlignment?")

\new StaffGroup <<
  %% Issue 6963 crash conditions:
  %% * at least 2 contexts have bar lines
  %% * at least 1 context has a vertical axis group index
  %% * no context with a vertical axis group index also has bar lines
  \new Staff \with { \remove "Axis_group_engraver" } s1
  \new Staff \with { \remove "Axis_group_engraver" } s1
  \new Staff \with { \remove "Bar_engraver" } s1
>>
