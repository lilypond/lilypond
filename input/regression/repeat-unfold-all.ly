\version "2.3.16"

\header { texidoc = "Volta repeats may be unfolded through the music
    function @code{\unfoldrepeats}."

}

nots = \relative c'   {
    c4 \repeat volta 2 c4 \alternative { d e  }
}
\paper {raggedright = ##t} 

\context Voice {
    \nots
    \bar "||"
    \unfoldrepeats \nots
}

