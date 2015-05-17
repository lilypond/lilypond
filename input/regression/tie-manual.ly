\header {

  texidoc = "Tie formatting may be adjusted manually, by setting the
@code{tie-configuration} property. The override should be placed at
the second note of the chord.

You can leave a Tie alone by introducing a non-pair value
(eg. @code{#t}) in the @code{tie-configuration} list.

"

}

\version "2.19.21"

\layout {
  ragged-right = ##t
}


\relative {
  
  <b' d f g>~

  
  \once \override TieColumn.tie-configuration =
     #'((0 . -1)  (2 . -1) (5.5 . 1) (7 . 1))

  <b d f g>
} 
