\version "2.3.22"

\header {

    texidoc = "If @code{raggedlast} is set, the systems are broken
    similar to paragraph formatting in text: the last line is
    justified. "

	  }


\score  {
     \relative c'' \new Staff { \repeat unfold 20 { c1 } }
    \layout{
	raggedlast = ##t
    }
}
	
