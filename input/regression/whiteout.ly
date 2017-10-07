\header {

  texidoc = "The whiteout command underlays a white background under a
markup.  The shape is determined by @code{whiteout-style}. The default
is @code{box} which produces a rectangle.  @code{rounded-box} produces
a rounded rectangle.  @code{outline} approximates the outline of the
markup."

}
\version "2.21.0"

\paper
{
  ragged-right = ##t
}

\relative {
  \override TextScript.layer = #'2
  \override TextScript.extra-offset = #'(2 . 4)
  c''4-\markup { \whiteout foo } c
  c-\markup { \whiteout \pad-markup #0.5 foo } c
  c-\markup {
    \override #'(thickness . 2)
    \whiteout foo
  }
  c
  c-\markup {
    \override #'((thickness . 3) (style . rounded-box))
    \whiteout foo
  }
  c
  c-\markup {
    \override #'((thickness . 3) (style . outline))
    \whiteout foo
  }
  c
}
