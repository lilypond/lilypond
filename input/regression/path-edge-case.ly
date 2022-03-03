\version "2.23.7"

\header {
  texidoc = "Ensure that certain paths are drawn correctly and do not
cause division by zero."
}

\markup \path #0.5 #'((moveto 0 0) (curveto 1 1 2 1 3 0))
