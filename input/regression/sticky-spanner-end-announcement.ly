\version "2.23.4"

\header {
  texidoc = "Sticky spanners have their end announced as soon as
their host's is announced."
}

#(set-default-paper-size "a7landscape")

\book {
  \new Score \with {
    \consists
      #(lambda (context)
         (let ((saw-hairpin-end #f)
               (saw-footnote-end #f))
           (make-engraver
             (end-acknowledgers
               ((hairpin-interface engraver grob source-engraver)
                  (set! saw-hairpin-end #t))
               ((footnote-interface engraver grob source-engraver)
                  (set! saw-footnote-end #t)))
             ((stop-translation-timestep engraver)
                (if (not (eq? saw-hairpin-end saw-footnote-end))
                    (ly:error "Sticky grob's end not announced with its hosts"))
                (set! saw-hairpin-end #f)
                (set! saw-footnote-end #f)))))
  }
  {
    s8\footnote #'(-2 . -2) "note" \< s8\!
  }
}
