\version "2.17.6"

\header {

  texidoc = "The thickness setting between markup lines and other
  lines is consistent."

}

\new Staff {
  \override TextSpanner.bound-details.right.text =
        #(markup #:draw-line '(0 . -1))
  \override TextSpanner.thickness = #2
  c'4 \startTextSpan
  c'4 \stopTextSpan
}
