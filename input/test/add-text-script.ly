\version "1.5.68"
\header {
texidoc= "Using make-music, you can add
various stuff to notes. Here is an example
how to add an extra fingering. 

In general, first do a display of the music you want ot
create, then write a function that will build the structure for you."
} 

#(define (make-text-script x) 
   (let ((m (ly-make-music "Text_script_req")))
     (ly-set-mus-property! m 'text-type 'finger)
     (ly-set-mus-property! m 'text x)
     m))
     
#(define (add-text-script m x)
   (if (equal? (ly-music-name m) "Request_chord")
       (ly-set-mus-property! m 'elements
			    (cons (make-text-script x)
				  (ly-get-mus-property m 'elements)))
       
       (let ((es (ly-get-mus-property m 'elements))
	     (e (ly-get-mus-property m 'element)) )
	 (map (lambda (y) (add-text-script y x)) es)
	 (if (music? e)
	     (add-text-script e x))))
   m)

\score {
  \apply #(lambda (x) (add-script x "6") (display x) x ) \notes { c4-3 }
}

