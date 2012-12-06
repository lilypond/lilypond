\version "2.17.6"

\header {
  texidoc = "The functions passed to the @code{positions} property should
handle complicated cases in the same manner that they handle more normal
cases.
"
}

\paper { ragged-right = ##t }
{
  r2.
  \override Beam.breakable = ##t
  r8[ g' \break a' r]
}
{
  r2.
  \override Beam.positions = #beam::align-with-broken-parts
  \override Beam.breakable = ##t
  r8[ g' \break a' r]
}
{
  r2.
  \override Beam.positions = #beam::slope-like-broken-parts
  \override Beam.breakable = ##t
  r8[ g' \break a' r]
}