\version "1.7.18"
\header { texidoc = "@cindex Repeat Manual
You can manually control repeat signs and numbers to produce
unusual output. "
}

\score { \notes \relative c'' {
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
	\paper{raggedright=##t}
}

