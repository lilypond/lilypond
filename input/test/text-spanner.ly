\score{
    \notes\relative c''{
        \property Voice.TextSpanner \push #'edge-text = #'("8 " . "")
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"

        \property Voice.TextSpanner \pop #'type
        \property Voice.TextSpanner \push #'type = #"dotted-line"
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"

        \property Voice.TextSpanner \pop #'type
        \property Voice.TextSpanner \push #'type = #"dashed-line"
        \property Voice.TextSpanner \push #'edge-height = #'(1 . -2)
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"


        \property Staff."c0-position" = #-13

        \property Voice.TextSpanner \push #'dash-length = #5
        \property Voice.TextSpanner \push #'line-thickness = #10
        a \spanrequest \start "text"
	b c 
        a \spanrequest \stop "text"
        \property Staff."c0-position" = #-6
    }
}
