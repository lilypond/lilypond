\version "2.19.21"

\header { texidoc = "Volta repeats may be unfolded through the music
    function @code{\\unfoldRepeats}."

}

nots = \relative   {
    c'4 \repeat volta 2 c4 \alternative { d e  }
}
\layout {ragged-right = ##t} 

\context Voice {
    \nots
    \bar "||"
    \unfoldRepeats \nots
}

