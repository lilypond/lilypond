\version "2.1.26"

\header { texidoc="@cindex Trill
Show trill line type.

The extended trill is a TextSpanner in @code{trill} style.
"
}


\score {
  \context RhythmicStaff \notes {
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


