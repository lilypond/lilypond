\version "2.25.35"

\header {

  texidoc = "If @code{ragged-last} is set, the systems are broken
    similar to paragraph formatting in text: the last line is
    unjustified. "

}

\layout{
  ragged-last = ##t
}

\relative \new Staff { \*20 c''1 }
