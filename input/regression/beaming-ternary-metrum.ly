
\version "2.1.28"
\header {
texidoc = "Automatic beaming works also in ternary time sigs."
}

\score {
   \notes \relative c'' {
\time 6/8
 c8.[ c16 c16 c16] 
}
\paper { raggedright = ##t}
}

