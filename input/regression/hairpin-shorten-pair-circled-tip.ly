\version "2.19.55"

\header {
  texidoc = "The @code{shorten-pair} property works with circled-tip
hairpins.  When two hairpins share a circle, the adjoining ends are
not moved.
"
}

{
  \override Hairpin.circled-tip = ##t
  \once \override Hairpin.shorten-pair = #'(-2 . -4)
  c'1~\<
  c'2~ c'\!
  \once \override Hairpin.shorten-pair = #'(0 . -4)
  c'1~\>
  c'2~ c'\!
  \break
  \override Hairpin.shorten-pair = #'(4 . -8)
  c'2~\> c'2~\<
  c'2~ c'2\!
}
