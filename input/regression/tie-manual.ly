\header {

  texidoc = "Tie formatting may be adjusted manually, by setting the
@code{tie-configuration} property."

}

\version "2.7.7"

\layout {
  raggedright = ##t
}


\relative c'' {
  \override TieColumn #'tie-configuration =
  #'((0 . -1)  (2 . -1) (5.5 . 1) (7 . 1))
  
  <b d f g>~  <b d f g>
} 
