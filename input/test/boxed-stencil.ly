
\version "2.3.22"
\header {
texidoc = "@cindex Boxed Stencil

The @code{print-function} can be overridden to draw a box around an arbitrary 
grob. " }


 \score {  \relative c''  {

 \override TextScript  #'print-function =
   #(make-stencil-boxer 0.1 0.3 0.2 Text_interface::print)

   c'4^"foo"

\override Stem  #'print-function =
   #(make-stencil-boxer 0.05 0.25 0.25 Stem::print)

\override Score.RehearsalMark  #'print-function =
   #(make-stencil-boxer 0.15 0.3 0.3 Text_interface::print)
      c8
\revert Stem #'print-function

      c4. c4 \mark "F" c1 
   }
\layout{raggedright = ##t}
}

