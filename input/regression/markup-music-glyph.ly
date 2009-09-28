\version "2.12.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Reset fontname for musicglyph.
For unknown glyphs, we print a warning."

}

{
  c'^\markup {
    \override #'(font-name . "Sans") {
      c'est un B \flat \musicglyph #"UNKNOWN-GLYPH"
    }
    % to get \flat, do:
    %  \normal-text \flat
  }
}
