
\version "2.1.7"

\header { texidoc = "@cindex text spanner
You can make LilyPond print text spanners. "
}

\score{
    \notes\relative c''{
        \property Voice.TextSpanner \override #'edge-text = #'("bla" . "blu")
        a \startTextSpan
	b c 
        a \stopTextSpan

        \property Voice.TextSpanner \override #'dash-period = #2
        \property Voice.TextSpanner \override #'dash-fraction = #0.0
        a \startTextSpan
	b c 
        a \stopTextSpan

        \property Voice.TextSpanner \revert #'style
        \property Voice.TextSpanner \override #'style = #'dashed-line
        \property Voice.TextSpanner \override #'edge-height = #'(1 . -2)
        a \startTextSpan
	b c 
        a \stopTextSpan


        \property Staff.centralCPosition = #-13

        \property Voice.TextSpanner \override #'dash-period = #10
        \property Voice.TextSpanner \override #'dash-fraction = #.5
        \property Voice.TextSpanner \override #'thickness = #10
        a \startTextSpan
	b c 
        a \stopTextSpan
        \property Staff.centralCPosition = #-6	
    }
	\paper{ raggedright = ##t}
}

