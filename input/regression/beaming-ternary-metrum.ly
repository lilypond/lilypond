\version "1.7.18"
\header {
texidoc = "automatic beaming also works in ternary time sigs."
}

\score {
   \notes \relative c'' {
\time 6/8
 c8.-[ c16 c16 c16] 
}
\paper { raggedright = ##t}
}

