
\version "2.1.26"
\header { texidoc = "@cindex Repeat Manual
By controlling manually the signs and numbers in repeats, an unusual 
output can be produced. "
}

\score { \notes \relative c'' {
% First a normal looking repeat:
 c2 c
    \set Score.repeatCommands = #'((volta "1."))
 c c
    \set Score.repeatCommands = #'((volta #f) end-repeat (volta "2."))
 c c
    \set Score.repeatCommands = #'((volta #f))
% Then a more strange one:
 c c
    \set Score.repeatCommands = #'((volta "93") end-repeat)
 c c
    \set Score.repeatCommands = #'((volta #f))
 c c
}
	\paper{raggedright=##t}
}

