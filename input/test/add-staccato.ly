
\version "2.1.7"

\header {

texidoc= "@cindex Add Stacato
Using make-music, you can add various stuff to notes. Here
is an example how to add staccato dots.  Note: for this simple case
one would not use scm constructs.  See separate-staccato.ly first.
"
} 

#(define (make-script x)
   (let ((m (make-music-by-name 'ArticulationEvent)))
     (ly:set-mus-property! m 'articulation-type x)
     m))
    
#(define (add-script m x)
   (if
    (equal? (ly:get-mus-property m 'name) 'EventChord)
    (ly:set-mus-property! m 'elements
			  (cons (make-script x)
				(ly:get-mus-property m 'elements))))
   m)

#(define (add-staccato m)
   (add-script m "staccato"))

\score {
  \notes\relative c'' {
    a b \apply #(lambda (x) (music-map add-staccato x)) { c c } 
  }
  \paper{ raggedright = ##t }
}


