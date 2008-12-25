\header {

    texidoc = "When merging heads, the dots are merged too."
    }
\layout { ragged-right = ##t }

\version "2.12.0"

{
  \relative c'' \new Staff {
    << { d8. e16 } \\ { d8. b16 } >> 
  }
}


