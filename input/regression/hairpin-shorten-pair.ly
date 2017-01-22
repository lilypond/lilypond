\version "2.19.55"

\header {
  texidoc = "The ends of hairpins may be offset with the
@code{shorten-pair} property.  Positive values offset ends to the right,
negative values to the left.
"
}

hairpin = {
  c'1~\<
  c'2~ c'\!
}

{
  \hairpin
  \once \override Hairpin.shorten-pair = #'(2 . 2)
  \hairpin
  \once \override Hairpin.shorten-pair = #'(-2 . -2)
  \hairpin
  \break
  \alterBroken shorten-pair #'((10 . 0) (-2 . -20)) Hairpin
  c'1~\<
  \break
  c'2~ c'\!
}
