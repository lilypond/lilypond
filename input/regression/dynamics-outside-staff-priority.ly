\version "2.17.16"

\header {
  texidoc = "@code{DynamicText}, @code{DynamicLineSpanner}, and
@code{Hairpin} do not have @code{outside-staff-priority} in @code{Dynamics}
contexts. This allows grobs with @code{outside-staff-priority} set
to be positioned above and below them.
"
}

<<
  \new Staff = "Test" {
    \tempo "Andante" c'1
  }
  \new Dynamics \with { alignAboveContext = "Test" } {
    s1\f
  }
>>
