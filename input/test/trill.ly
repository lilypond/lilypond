\version "2.3.8"

\header { texidoc="@cindex Trill
The extended trill may be produced using @code{TextSpanner} with @code{trill} 
spanner style.
"
}


\score {
  \context RhythmicStaff  {
    \stemDown
    \override Stem  #'transparent = ##t
    \override TextSpanner  #'dash-fraction = #0.0
    \override TextSpanner  #'dash-period = #1.0
    \override TextSpanner  #'edge-height = #'(0 . 1.5)
    \override TextSpanner  #'edge-text = #'("bla " . "")
    a\startTextSpan b c a\stopTextSpan


    %% TODO: should have trill spanner.
    \override TextSpanner  #'style = #'trill
    \override TextSpanner  #'edge-height = #'(0 . 0)
    \override TextSpanner  #'edge-text
  = #(cons (make-musicglyph-markup "scripts-trill")  "")
    a\startTextSpan b c a\stopTextSpan
  }
	\paper { raggedright = ##t} 
}


