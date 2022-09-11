\version "2.23.13"

\header {
  lsrtags = "contemporary-notation, winds"

  texidoc = "
It is possible to indicate special articulation techniques such as a
flute @qq{tongue slap} by replacing the note head with the appropriate
glyph.  For that we can draw the accent-like note head with
@code{\\markup}.
"

  doctitle = "Flute slap notation"
}


slap =
#(define-music-function (music) (ly:music?)
#{
  \temporary \override NoteHead.stencil = #ly:text-interface::print
  \temporary \override NoteHead.text =
    \markup 
    \translate #'(1 . 0) 
    \override #'(thickness . 1.4) 
    \overlay {
      \draw-line #'(-1.2 . 0.4)
      \draw-line #'(-1.2 . -0.4)
    }
  \temporary \override NoteHead.stem-attachment =
  #(lambda (grob)
     (let* ((stem (ly:grob-object grob 'stem))
            (dir (ly:grob-property stem 'direction UP))
            (is-up (eqv? dir UP)))
       (cons dir (if is-up 0 -0.8))))
  #music
  \revert NoteHead.stencil
  \revert NoteHead.text
  \revert NoteHead.stem-attachment
#})

\relative c' {
  c4 \slap c d r
  \slap { g4 a } b r
}
