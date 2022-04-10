\version "2.23.8"

\header {
  texidoc = "Ensure that certain paths are drawn correctly and do not
cause division by zero."
}

\markup \path #0.5 #'((curveto 1 1 2 1 3 0))
