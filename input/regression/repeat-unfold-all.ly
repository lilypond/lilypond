\version "2.3.1"

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
	\applymusic #unfold-repeats \nots
    }
    \paper {raggedright = ##t} 
}

