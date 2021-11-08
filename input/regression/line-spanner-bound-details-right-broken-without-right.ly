\version "2.23.5"

\header {
  texidoc = "The absence of @code{left} or @code{right} in the
@code{bound-details} of a line spanner combined with the presence
of non-empty @code{left-broken} or @code{right-broken} should
not cause an error."
}


{
  \override Glissando.breakable = ##t
  \override Glissando.bound-details = #'((right-broken . ((text . "text"))))
  c1\glissando \break c'
}
