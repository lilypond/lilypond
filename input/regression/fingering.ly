\version "1.7.18"
\header {

texidoc = "Automatic fingering tries to put fingering instructions
next to noteheads.
"
}

    \paper { raggedright= ##t }


\score {

 \notes\relative c' {
 c4-4
  <<c-1 f-4>>
  << c-1 e-2 g-3  b-4 >>
}}
 


