\header {

    texidoc = "When merging heads, the dots are merged too."
    }
\version "2.3.17"

\score {
 {
\relative c'' \new Staff {
    << { d8. e16 } \\ { d8. b16 } >> 
    }
}

\paper { raggedright = ##t }}
