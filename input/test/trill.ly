\version "2.1.7"

\header { texidoc="@cindex Trill
Show trill line type.

The extended trill is a TextSpanner in @code{trill} style.
"
}


\score {
  \context RhythmicStaff \notes {
    \stemDown
    \property Voice.Stem \override #'transparent = ##t
    \property Voice.TextSpanner \set #'dash-fraction = #0.0
    \property Voice.TextSpanner \set #'dash-period = #1.0
    \property Voice.TextSpanner \set #'edge-height = #'(0 . 1.5)
    \property Voice.TextSpanner \set #'edge-text = #'("bla " . "")
    a\startTextSpan b c a\stopTextSpan


    %% TODO: should have trill spanner.
    \property Voice.TextSpanner \set #'style = #'trill
    \property Voice.TextSpanner \set #'edge-height = #'(0 . 0)
    \property Voice.TextSpanner \set #'edge-text
     = #(cons (make-musicglyph-markup "scripts-trill")  "")
    a\startTextSpan b c a\stopTextSpan
  }
	\paper { raggedright = ##t} 
}


