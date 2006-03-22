\header {
  texidoc = "Arrows can be applied to text-spanners and line-spanners (such as the Glissando)"
}

\version "2.8.0"

\paper {
  ragged-right = ##t
}

\relative c'' {
  \override TextSpanner #'edge-text = #'("foo" . "bar")
  \override TextSpanner #'bound-padding = #1.0
  \override TextSpanner #'dash-fraction = #'()
  \override TextSpanner #'arrow = ##t
  
  \override Glissando #'arrow = ##t
  \override Glissando #'arrow-length = #0.5
  \override Glissando #'arrow-width = #0.25
  
  a8\startTextSpan gis8 a4 b4\glissando
  b,4 | g' c\stopTextSpan c
}
