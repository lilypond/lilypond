\header{
    texidoc =

    "Extenders that end a staff should not extend past the staff.
Also shown: a trick to get an extender at the end of the staff.
"

}

sopran = \notes \relative c'' {
\time 3/4 a2.( | \break
)g2 < g4 { s8 s8 } > |
}

text = \lyrics {
vielt __ Zeit. __ " " 
}

\score {
<
\addlyrics
  \context Staff \sopran
  \context Lyrics \text
>
\paper { linewidth = 5.0\cm
}
}
