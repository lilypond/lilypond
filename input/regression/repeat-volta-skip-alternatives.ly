\version "1.7.18"
\header {
    texidoc = "When too few alternatives are present, the first alternative is repeated,
by printing a range for the 1st repeat."
    }


\score {  \notes \context Voice {
  \repeat volta 3 c1
    \alternative { d f } e4 } } 

