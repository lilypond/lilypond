\version "1.5.68"

\header {
texidoc = "ottava spanners."
}

\score{
    \notes\relative c''{
        \property Voice.TextSpanner \override #'edge-text = #'("8 " . "")
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"

        \property Voice.TextSpanner \revert #'type
        \property Voice.TextSpanner \override #'type = #'dotted-line
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"

        \property Voice.TextSpanner \revert #'type
        \property Voice.TextSpanner \override #'type = #'dashed-line
        \property Voice.TextSpanner \override #'edge-height = #'(1 . -2)
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"


        \property Staff.centralCPosition = #-13

        \property Voice.TextSpanner \override #'dash-length = #5
        \property Voice.TextSpanner \override #'thickness = #10
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"
        \property Staff.centralCPosition = #-6
    }
}
