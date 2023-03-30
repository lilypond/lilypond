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
  %% Use a targeted tweak here (TupletBracket.shorten-pair rather than
  %% shorten-pair) because a direct tweak also modifies TupletNumber,
  %% leading to a warning from -dcheck-internal-types in make check.
  \tweak TupletBracket.shorten-pair #'(-1 . -1)
    \tuplet 5/4 { c''16 c'' c'' c'' c'' }
  \tweak TupletBracket.shorten-pair #'(1 . 1)
    \tuplet 5/4 { c''16 c'' c'' c'' c'' }
  \tweak TupletBracket.shorten-pair #'(-1 . 1)
    \tuplet 5/4 { c''16 c'' c'' c'' c'' }
}
