
\version "2.10.0"


\header {

  texidoc = "

@cindex Embedded Postscript

The markup command @code{\postscript} inserts postscript directly into
the output."

}

\layout {
  line-width = 70 * 5 \pt
}

\relative c'' {
  a-\markup { \postscript #"3 4 moveto 5 3 rlineto stroke" }
  -\markup { \postscript #"[ 0 1 ] 0 setdash 3 5 moveto 5 -3 rlineto stroke " }
  
  b-\markup { \postscript #"3 4 moveto 0 0 1 2 8 4 20 3.5 rcurveto stroke" }
  s2
  a'1
}

