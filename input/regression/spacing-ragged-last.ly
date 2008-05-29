\version "2.10.0"

\header {

  texidoc = "If @code{ragged-last} is set, the systems are broken
    similar to paragraph formatting in text: the last line is
    unjustified. "

}

\layout{
  ragged-last = ##t
}

\relative c'' \new Staff { \repeat unfold 20 { c1 } }

