\header {

  texidoc =

  "With @code{music-map}, you can apply functions operating on a
single piece of music to an entire music expression. In this example,
the function @code{notes-to-skip} changes a note to a skip. When
applied to an entire music expression in the 1st measure, the scripts
and dynamics are left over. These are put onto the 2nd measure."

}

\version "2.17.27"

#(define (notes-to-skip m)
  "Convert all stuff with duration (notes, lyrics, bass figures, etc.) to skips.
Scripts and dynamics are maintained."
  (if (music-is-of-type? m 'rhythmic-event)
      (make-music 'SkipEvent m)
      m))


\layout { ragged-right= ##t }

foobar =  \transpose c c' { c4\>-^ c4-^ c4\!-^ c4-^  }


\relative c''  \context Voice {
  \foobar

  << \applyMusic #(lambda (x) (music-map notes-to-skip x))
     \foobar
     { d8 d d d d d d d  } >>
}
