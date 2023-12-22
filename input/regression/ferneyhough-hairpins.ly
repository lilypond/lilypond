\version "2.25.12"

\header {
  texidoc = "LilyPond creates hairpins found in Ferneyhough scores.  Flared
hairpins may print circled tips.
"
}

mus = {
  a4\< a a a\f
  a\p\< a a a\ff
  a\sfz\< a a a\!
}

<<
  \new Staff \relative a' {
    \override Hairpin.stencil = #flared-hairpin
    \mus
    \override Hairpin.stencil = #constante-hairpin
    \mus
    \override Hairpin.stencil = #flared-hairpin
    \mus
    \override Hairpin.stencil = #constante-hairpin
    \mus
  }

  %% circled tips with `constante-hairpin' makes no sense, thus always unset
  \new Staff \relative a' {
    \override Hairpin.stencil = #flared-hairpin
    \override Hairpin.circled-tip = ##t
    \mus
    \override Hairpin.stencil = #constante-hairpin
    \override Hairpin.circled-tip = #'()
    \mus
    \override Hairpin.stencil = #flared-hairpin
    \override Hairpin.circled-tip = ##t
    \mus
    \override Hairpin.stencil = #constante-hairpin
    \override Hairpin.circled-tip = #'()
    \mus
  }
>>
