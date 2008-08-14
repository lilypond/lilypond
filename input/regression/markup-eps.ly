\header {

  texidoc = "The epsfile markup command reads an EPS file"

}
\version "2.11.51"

#(let* ((port (open-output-file "box.eps")))

  (display "%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 5 5 105 105
10 setlinewidth 10 10 moveto 0 90 rlineto 90 0 rlineto 0 -90 rlineto
closepath stroke" port)

  (close port))

{ c''4-\markup { \box \epsfile #X #10 #"box.eps" } } 
