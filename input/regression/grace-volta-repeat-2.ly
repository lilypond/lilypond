\version "2.3.4"

\header {

    texidoc = "A volta repeat may begin with a grace. Consecutive ending and
    starting repeat bars are into one @code{:||:}."

      }

    \paper { raggedright= ##t }

\score {\relative c' {
\repeat volta 2 {
        c1 
}
\repeat volta 2 {
        \grace {c8 } c4
}
}}
