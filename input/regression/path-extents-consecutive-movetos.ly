\version "2.23.11"

\header {
  texidoc = "The extents of a path stencil are correctly
computed when it contains consecutive @code{moveto}
commands."
}

\markup \box \path #0.5
  #'((moveto 1 1)
     (rmoveto 2 2) 
     (lineto 4 4))
