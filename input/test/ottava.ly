
\version "1.3.117";

\score{
    \notes\relative c'''{
        \property Voice.TextSpanner \revert #'type
        \property Voice.TextSpanner \override #'type = #'dotted-line
        \property Voice.TextSpanner \override #'edge-height = #'(0 . 1.5)
        \property Voice.TextSpanner \override #'edge-text = #'("8va " . "")
        \property Staff.centralCPosition = #-13

        a\spanrequest \start "text" b c a \spanrequest \stop "text"

        \property Staff.centralCPosition = #-6
	a b c a

        \property Staff.centralCPosition = #1
        \property Voice.TextSpanner \override #'edge-text = #'("8bass " . "")
        \property Voice.TextSpanner \override #'direction = #-1
        a\spanrequest \start "text" b c a \spanrequest \stop "text"
    }
}
