\version "1.7.0"  %% or actually: 1.7.1 ...
\header {

texidoc= "Using make-music, you can add various stuff to notes. Here
is an example how to add staccato dots.  Note: for this simple case
one would not use scm constructs.  See separate-staccato.ly first."

} 

#(define (make-script x)
   (let ((m (make-music-by-name "Articulation_req")))
     (ly-set-mus-property! m 'articulation-type x)
     m))
    
#(define (add-script m x)
   (if (equal? (make-music-by-name m) "Request_chord")
       (ly-set-mus-property! m 'elements
			    (cons (make-script x)
				  (ly-get-mus-property m 'elements)))

       (let ((es (ly-get-mus-property m 'elements))
	     (e (ly-get-mus-property m 'element)) )
	 (map (lambda (y) (add-script y x)) es)
	 (if (music? e)
	     (add-script e x))))
   m)

#(define (add-staccato m)
   (add-script m "staccato"))

\score {
  \notes\relative c'' {
    a b \apply #add-staccato { c c } 
    a b \apply #add-staccato { c c } 
  }
}

