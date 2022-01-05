\version "2.23.6"

\header {
  texidoc = "The @code{show-horizontal-skylines} and
@code{show-horizontal-skylines} properties display
skylines to assist debugging."
}

{
  \override Staff.Clef.show-vertical-skylines = ##t
  \override Accidental.show-horizontal-skylines = ##t
  cis'
}
