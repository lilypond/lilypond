\version "2.19.21"
\header {

    texidoc = "When too few alternatives are present, the first
alternative is repeated, by printing a range for the 1st repeat."

}

\paper { ragged-right = ##t } 


\relative \context Voice {
  \repeat volta 3 c''1
    \alternative { d f } e4
} 

