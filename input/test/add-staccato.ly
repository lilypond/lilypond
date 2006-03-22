
\version "2.8.0"

\header {

texidoc= "@cindex Add Stacato
Using @code{make-music}, you can add various stuff to notes. In this
example staccato dots are added to the notes."
} 

#(define (make-script x)
   (make-music 'ArticulationEvent
               'articulation-type x))
    
#(define (add-script m x)
   (if
     (equal? (ly:music-property m 'name) 'EventChord)
     (set! (ly:music-property m 'elements)
           (cons (make-script x)
                 (ly:music-property m 'elements))))
   m)

#(define (add-staccato m)
   (add-script m "staccato"))

\score {
  \relative c'' {
    a b \applyMusic #(lambda (x) (music-map add-staccato x)) { c c } 
  }
  \layout{ ragged-right = ##t }
}


