#(ly:set-option 'old-relative)
\header {

texidoc = "Tieing only parts of chords is possible. It requires
putting the Tie engraver at Thread level, and redirecting untied notes
to a different thread."

}
\version "1.9.0"
    \paper { raggedright= ##t }

\score {

\notes \context Thread \relative c' {
  < c-~ e \context Thread = "untied" c' > 
  < c e c'  > 
}


\paper {
\translator {
  \ThreadContext
  \consists "Tie_engraver"
  \consists "New_tie_engraver"
}
\translator { \VoiceContext
  \remove "Tie_engraver"
  \remove "New_tie_engraver"
}
}}

 
