  
#(define (voicify-list lst number)
   "Make a list of Musics.

   voicify-list :: [ [Music ] ] -> number -> [Music]
   LST is a list music-lists.
"

   (if (null? lst) '()
       (cons (context-spec-music
	      (make-sequential-music
	       (list
		(make-voice-props-set number)
		(make-simultaneous-music (car lst))))

	      "Voice"  (number->string number))
	      (voicify-list (cdr lst) (+ number 1))
       ))
   )

#(define (voicify-chord ch)
  "Split the parts of a chord into different Voices using separator"
   (let* ((es (ly-get-mus-property ch 'elements)))


     (ly-set-mus-property! ch 'elements
       (voicify-list (split-list es music-separator?) 0))
     ch
   ))

#(define (voicify-music m)
   "Recursively split chords that are separated with \\ "
   
   (if (not (music? m))
       (begin (display m)
       (error "not music!"))
       )
   (let*
       ((es (ly-get-mus-property m 'elements))
	(e (ly-get-mus-property m 'element))
	)
	
     (if
      (and (equal? (ly-music-name m) "Simultaneous_music")
	   (reduce (lambda (x y ) (or x y)) 	(map music-separator? es)))
      (voicify-chord m)
      (begin
	(if (pair? es)
	    (ly-set-mus-property! m 'elements (map voicify-music es)))
	(if (music? e)
	    (ly-set-mus-property! m 'element  (voicify-music e)))
	    
	m)
      
      )
     ))

\score { \notes \context Staff \relative c'' 
\apply #voicify-music {
   c4   <g' \\ c, \\ f \\ d > f g < c \\ d> a 
}
}


