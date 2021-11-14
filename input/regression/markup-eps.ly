\header {

  texidoc = "The epsfile markup command reads an EPS file"

}
\version "2.21.0"

#(let* ((port (open-output-file "box.eps")))

  (display "%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 5 5 205 105
10 setlinewidth
10 10 moveto 0 90 rlineto 190 0 rlineto 0 -90 rlineto
closepath stroke
25 25 moveto
/Helvetica 15 selectfont
1 0 0 setrgbcolor
(LL) show 
" port)

  (close port))

{ c''4-\markup { \box \epsfile #X #10 "box.eps" } } 
