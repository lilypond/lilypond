\header {

    texidoc = "When merging heads, the dots are merged too."
    }
\version "2.4.0"

\score {
 {
\relative c'' \new Staff {
    << { d8. e16 } \\ { d8. b16 } >> 
    }
}

\layout { raggedright = ##t }}
