\version "2.3.4"
\header {

    texidoc = "When too few alternatives are present, the first
alternative is repeated, by printing a range for the 1st repeat."

}


\score {   \relative c'' \context Voice {
  \repeat volta 3 c1
    \alternative { d f } e4 } } 

