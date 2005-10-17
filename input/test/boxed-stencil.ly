
\version "2.7.13"
\header {
  texidoc = "@cindex Boxed Stencil

The @code{print-function} can be overridden to draw a box around an arbitrary 
grob. " }


\layout{raggedright = ##t}
\relative c''  {

  \override TextScript #'callbacks #'stencil =
  #(make-stencil-boxer 0.1 0.3 Text_interface::print)

  c'4^"foo"

  \override Stem #'callbacks #'stencil =
  #(make-stencil-boxer 0.05 0.25 Stem::print)

  \override Score.RehearsalMark #'callbacks #'stencil =
  #(make-stencil-boxer 0.15 0.3 Text_interface::print)
  b8
  \revert Stem #'callbacks % stencil


  c4. c4 \mark "F" c1 
}

