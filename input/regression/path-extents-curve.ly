\version "2.23.11"

\header {
  texidoc = "Curve path stencils have correct extents."
}

\markup \box \path #0.5 #'((curveto 0 1 1 1 2 1))
