\version "2.25.12"

\header {
  texidoc = "The @code{shorten-pair} property works with circled-tip
hairpins.  When two hairpins share a circle, the adjoining ends are
not moved.  The same holds, if @code{flared-hairpin} is used to get hairpins in
the style of Ferneyhough.
"
}

mus = {
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

<<
  \new Staff \mus
  \new Staff { \override Hairpin.stencil = #flared-hairpin \mus }
>>
