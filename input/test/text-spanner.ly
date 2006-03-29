
\version "2.7.39"

\header { texidoc = "@cindex text spanner
Text spanners can be used in the similar manner than markings for pedals
or octavation.
"
      }
\layout{ ragged-right = ##t}

\relative c''{
    \override TextSpanner  #'edge-text = #'("bla" . "blu")
    a \startTextSpan
    b c 
    a \stopTextSpan

    \override TextSpanner  #'dash-period = #2
    \override TextSpanner  #'dash-fraction = #0.0
    a \startTextSpan
    b c 
    a \stopTextSpan

    \revert TextSpanner #'style
    \override TextSpanner  #'style = #'dashed-line
    \override TextSpanner  #'edge-height = #'(1 . -2)
    a \startTextSpan
    b c 
    a \stopTextSpan


    \set Staff.middleCPosition = #-13

    \override TextSpanner  #'dash-period = #10
    \override TextSpanner  #'dash-fraction = #.5
    \override TextSpanner  #'thickness = #10
    a \startTextSpan
    b c 
    a \stopTextSpan
    \set Staff.middleCPosition = #-6	
}

