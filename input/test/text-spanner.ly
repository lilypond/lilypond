
\version "1.3.110";

\score{
    \notes\relative c''{
        \property Voice.TextSpanner \override #'edge-text = #'("8 " . "")
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"

        \property Voice.TextSpanner \revert #'type
        \property Voice.TextSpanner \override #'type = #"dotted-line"
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"

        \property Voice.TextSpanner \revert #'type
        \property Voice.TextSpanner \override #'type = #"dashed-line"
        \property Voice.TextSpanner \override #'edge-height = #'(1 . -2)
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"


        \property Staff."c0-position" = #-13

        \property Voice.TextSpanner \override #'dash-length = #5
        \property Voice.TextSpanner \override #'line-thickness = #10
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"
        \property Staff."c0-position" = #-6
    }
}
