\version "2.21.0"

\header {
  texidoc = "Slurs may be used instead of brackets for tuplets through
the @code{tuplet-slur} property of @code{TupletBracket}.  Rules for
visibility are the same as for regular brackets, so
@code{bracket-visibility} should be set to @code{#t} if the slur is
desired for beamed groups.
"
}

{
  \override TupletBracket.tuplet-slur = ##t
  \tuplet 3/2 { c'4 e' g' }
  \tuplet 3/2 { g''4 e'' c'' }
  \override TupletBracket.bracket-visibility = ##t
  \tuplet 3/2 { c'8 e' g' }
  \tuplet 3/2 { c'8 c'''' c' }
  \tuplet 5/4 { c'''16 g'' e'' c'' g' }
  \tupletUp \tuplet 5/4 { c'''16 g'' e'' c'' g' }
}
