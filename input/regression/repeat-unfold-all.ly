\version "2.2.0"

\header { texidoc = "Volta repeats may be unfolded through the Scheme
    function @code{unfold-repeats}."

}

nots = \notes\relative c'   {
    c4 \repeat volta 2 c4 \alternative { d e  }
}

\score {
    \notes \context Voice {
	\nots
	\bar "||"
	\apply #unfold-repeats \nots
    }
    \paper {raggedright = ##t} 
}

