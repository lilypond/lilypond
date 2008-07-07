\header {

  texidoc = "Reset fontname for musicglyph. For unknown glyphs, we print a warning."

}

\version "2.11.51"

{
  c'^\markup
  {
    \override #'(font-name . "Sans")
    { c'est un B \flat \musicglyph #"UNKNOWN-GLYPH" }
				% to get \flat, do:
				%  \normal-text \flat
  }
}
