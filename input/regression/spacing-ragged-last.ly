\version "2.8.0"

\header {

  texidoc = "If @code{raggedlast} is set, the systems are broken
    similar to paragraph formatting in text: the last line is
    unjustified. "

}

\layout{
  raggedlast = ##t
}

\relative c'' \new Staff { \repeat unfold 20 { c1 } }

