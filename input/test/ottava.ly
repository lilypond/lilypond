\score{
    \notes\relative c'''{
        \property Voice.TextSpanner \pop #'type
        \property Voice.TextSpanner \push #'type = #"dotted-line"
        \property Voice.TextSpanner \push #'edge-height = #'(0 . 1.5)
        \property Voice.TextSpanner \push #'edge-text = #'("8va " . "")
        \property Staff.centralCPosition = #-13

        a\spanrequest \start "text" b c a \spanrequest \stop "text"

        \property Staff.centralCPosition = #-6
	a b c a

        \property Staff.centralCPosition = #1
        \property Voice.TextSpanner \push #'edge-text = #'("8bass " . "")
        \property Voice.TextSpanner \push #'direction = #-1
        a\spanrequest \start "text" b c a \spanrequest \stop "text"
    }
}
