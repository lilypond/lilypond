\header {
  texidoc = "If broken, Glissandi anticipate on the pitch of the next line."
  
}
\version "2.19.21"
\paper {
  ragged-right = ##T }

\relative {
  \override Glissando.breakable = ##t 
  d''1 \glissando |
  \break
  c,1
}
