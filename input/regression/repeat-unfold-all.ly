\version "2.3.4"

\header { texidoc = "Volta repeats may be unfolded through the Scheme
    function @code{unfold-repeats}."

}

nots = \relative c'   {
    c4 \repeat volta 2 c4 \alternative { d e  }
}

\score {
     \context Voice {
	\nots
	\bar "||"
	\applymusic #unfold-repeats \nots
    }
    \paper {raggedright = ##t} 
}

