\version "1.3.146"


\score { \notes {
 c4
    \property Score.repeatCommands = #'((volta "93") end-repeat)
 c4 c4
    \property Score.repeatCommands = #'((volta #f))
 c4 c4
}
}
