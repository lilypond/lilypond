
\version "2.4.0"
\header {
texidoc = "Automatic beaming works also in ternary time sigs."
}

\score {
    \relative c'' {
\time 6/8
 c8.[ c16 c16 c16] 
}
\layout { raggedright = ##t}
}

