\version "1.7.23"

\header {
texidoc= "@cindex make-music Fingering
You can add various stuff to notes using make-music.
Here is an example of how to add an extra fingering. 

In general, first do a display of the music you want to
create, then write a function that will build the structure for you.
"
} 

#(define (make-text-script x) 
   (let ((m (make-music-by-name 'TextScriptEvent)))
    (ly:set-mus-property! m 'direction DOWN) 
     (ly:set-mus-property! m 'text (make-simple-markup x))
     m))
     
#(define (add-text-script m x)
   (if (equal? (ly:get-mus-property m 'name) 'EventChord)
       (ly:set-mus-property! m 'elements
			    (cons (make-text-script x)
				  (ly:get-mus-property m 'elements)))
       
       (let ((es (ly:get-mus-property m 'elements))
	     (e (ly:get-mus-property m 'element)) )
	 (map (lambda (y) (add-text-script y x)) es)
	 (if (ly:music? e)
	     (add-text-script e x))))
   m)

\score {
  \apply #(lambda (x)  (add-text-script x "6") (display-music x) x ) \notes { c'4-3 }
	\paper{ raggedright = ##t }
}


