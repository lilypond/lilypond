\version "2.23.11"

\header {
  texidoc = "Exercise various situations in path stencils."
}

\markup \box \stencil
#(make-path-stencil
  '(lineto 0 1
    rlineto 1 0
    rlineto 0 1
    moveto 0 0
    rlineto -1 -1
    moveto 0 0
    moveto 3 0
    rlineto -1 -1
    curveto 3 -1 4 -1 5 0
    rcurveto 1 1 2 1 3 0
    closepath
    rlineto 2 -2)
  0.5
  2
  -1
  #f)
