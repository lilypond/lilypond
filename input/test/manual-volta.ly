\version "1.3.146"


\score { \notes {
% First a normal looking repeat:
 c2 c
    \property Score.repeatCommands = #'((volta "1."))
 c c
    \property Score.repeatCommands = #'((volta #f) end-repeat (volta "2."))
 c c
    \property Score.repeatCommands = #'((volta #f))
% Then a more strange one:
 c c
    \property Score.repeatCommands = #'((volta "93") end-repeat)
 c c
    \property Score.repeatCommands = #'((volta #f))
 c c
}
}
