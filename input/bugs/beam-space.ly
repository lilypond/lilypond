

u = { \translator Staff = up \stemDown }
m = { \translator Staff = mid \stemUp }

global = \notes { \key fis \major \time 6/8 }

righta = \notes \transpose c'' {
 \repeat unfold 4 { \m [a,16 \u d a d] \m [c \u d c' d ] [c \m b,] [d \u d ] } ]
}


\score { \notes
  \context PianoStaff <
    \context Staff = up {
      \clef G \global \righta
    }
    \context Staff = mid {
      \clef F \global s2. *4
    }
  >
  \paper { }
}
