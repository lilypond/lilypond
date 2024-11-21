\header {
  texidoc = "The @code{\\whiteout} command underlays a white background
under a markup.  The shape is determined by @code{whiteout-style}; the
default is @code{box}, which produces a rectangle.  @code{rounded-box}
produces a rounded rectangle, @code{outline} approximates the outline of the
markup.
"
}

\version "2.25.22"

\markup \combine
  \with-color \grey \filled-box #'(-1 . 70) #'(-2  . 7) #0
  \fontsize #10 \line {
    \whiteout foo
    \whiteout \pad-markup #0.5 foo
    \override #'(thickness . 3) \whiteout foo
    \override #'((thickness . 3) (style . rounded-box)) \whiteout foo
    \override #'((thickness . 3) (style . outline)) \whiteout foo
    \override #'((style . outline) (color . "red")) \whiteout foo
  }
