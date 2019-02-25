\header {
  texidoc = "Arrows can be applied to text-spanners and line-spanners (such as the Glissando)"
}

\version "2.21.0"

\paper {
  ragged-right = ##t
}

\relative {
  \override TextSpanner.bound-padding = #1.0
  \override TextSpanner.style = #'line
  \override TextSpanner.bound-details.right.arrow = ##t
  \override TextSpanner.bound-details.left.text = "fof"
  \override TextSpanner.bound-details.right.text = "gag"
  \override TextSpanner.bound-details.right.padding = #0.6

  \override TextSpanner.bound-details.right.stencil-align-dir-y = #CENTER
  \override TextSpanner.bound-details.left.stencil-align-dir-y = #CENTER
  
  \override Glissando.bound-details.right.arrow = ##t
  \override Glissando.arrow-length = #0.5
  \override Glissando.arrow-width = #0.25
  
  a'8\startTextSpan gis8 a4 b4\glissando
  b,4 | g' c\stopTextSpan c
}
