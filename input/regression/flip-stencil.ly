\version "2.19.19"

\header {
  texidoc = "Stencils can be flipped horizontally or vertically within
their bounding box using @code{flip-stencil}."
}

square =
#(make-path-stencil
  '(lineto 0 2 lineto 2 2 lineto 2 0 closepath)
  0.1 1 1 #f)

triangle =
#(stencil-with-color
  (make-path-stencil
   '(lineto 2 2 lineto 2 0 closepath)
   0.3 1 1 #f)
  blue)

{
  g'1^\markup \stencil
  #(ly:stencil-add square triangle)
  _\markup \teeny "baseline"

  g'1^\markup \stencil
  #(ly:stencil-add square (flip-stencil X triangle))
  _\markup \teeny "flip X"

  g'1^\markup \stencil
  #(ly:stencil-add square (flip-stencil Y triangle))
  _\markup \teeny "flip Y"
}
