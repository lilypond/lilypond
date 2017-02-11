\version "2.21.0"

\header {
  texidoc = "Tuplet slurs may be tweaked through the @code{shorten-pair}
and @code{dash-definition} properties.
"
}

{
  \override TupletBracket.bracket-visibility = ##t
  \override TupletBracket.tuplet-slur = ##t
  \override TupletBracket.dash-definition = #'((0 0.25 1 1)
                                               (0.3 0.7 0.4 0.75)
                                               (0.75 1.0 1 1))
  \tuplet 5/4 { c''16 c'' c'' c'' c'' }
  \tweak shorten-pair #'(-1 . -1) \tuplet 5/4 { c''16 c'' c'' c'' c'' }
  \tweak shorten-pair #'(1 . 1) \tuplet 5/4 { c''16 c'' c'' c'' c'' }
  \tweak shorten-pair #'(-1 . 1) \tuplet 5/4 { c''16 c'' c'' c'' c'' }
}
