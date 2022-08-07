\version "2.23.12"

\header {
  texidoc = "Trill spanners with @code{outside-staff-priority} turned off
do not collide with notes."
}

{
  \override TrillSpanner.staff-padding = ##f
  \override TrillSpanner.outside-staff-priority = ##f
  c'\startTrillSpan c'\stopTrillSpan
  c'''\startTrillSpan c'''\stopTrillSpan
}
