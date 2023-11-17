\version "2.25.11"

\header {
  texidoc = "The markup command @code{\\align-on-other} accepts both numbers
and @code{##f} as position arguments; the latter indicates that the object's
reference point should be used for alignment."
}

\markup {
  \column {
    \null
    \smaller \typewriter "#RIGHT 123 #LEFT 12345:"
    \smaller \typewriter "#RIGHT 123 #LEFT \\fermata:"
    \null
    \smaller \typewriter "#RIGHT 123 ##f \\fermata:"
    \smaller \typewriter "##f 123 #RIGHT \\fermata:"
    \null
  }
  \hspace #2
  \column {
    123
    \align-on-other #X #RIGHT 123
                       #LEFT 12345
    \align-on-other #X #RIGHT 123
                       #LEFT \fermata
    123
    \align-on-other #X #RIGHT 123
                       ##f \fermata
    \align-on-other #X ##f 123
                       #RIGHT \fermata
    123
  }
}
