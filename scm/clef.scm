
;; (name . (glyph clef-position octavation))
;;
;; -- the name clefOctavation is misleading. The value 7 is 1 octave, not 7 Octaves.

(define supported-clefs '(
	  ("treble" . ("clefs-G" -2 0))
	  ("violin" . ("clefs-G" -2 0))
	  ("G" . ("clefs-G" -2 0))
	  ("G2" . ("clefs-G" -2 0))
	  ("french" . ("clefs-G" -4  0))
	  ("soprano" . ("clefs-C" -4  0))
	  ("mezzosoprano" . ("clefs-C" -2  0))
	  ("alto" . ("clefs-C" 0 0))
	  ("C" . ("clefs-C" 0 0))
	  ("tenor" . ("clefs-C" 2 0))
	  ("baritone" . ("clefs-C" 4  0))
	  ("varbaritone"  . ("clefs-F" 0 0))
	  ("bass" . ("clefs-F" 2  0))
	  ("F" . ( "clefs-F" 2 0))
	  ("subbass" . ("clefs-F" 4 0))
          ("percussion" . ("clefs-percussion" 0 0))
          ("tab" . ("clefs-tab" 0 0))

	  ;; should move mensural stuff to separate file? 
	  ("vaticana_do1" . ("clefs-vaticana_do" -1 0))
	  ("vaticana_do2" . ("clefs-vaticana_do" 1 0))
	  ("vaticana_do3" . ("clefs-vaticana_do" 3 0))
	  ("vaticana_fa1" . ("clefs-vaticana_fa" -1 0))
	  ("vaticana_fa2" . ("clefs-vaticana_fa" 1 0))
	  ("medicaea_do1" . ("clefs-medicaea_do" -1 0))
	  ("medicaea_do2" . ("clefs-medicaea_do" 1 0))
	  ("medicaea_do3" . ("clefs-medicaea_do" 3 0))
	  ("medicaea_fa1" . ("clefs-medicaea_fa" -1 0))
	  ("medicaea_fa2" . ("clefs-medicaea_fa" 1 0))
	  ("hufnagel_do1" . ("clefs-hufnagel_do" -1 0))
	  ("hufnagel_do2" . ("clefs-hufnagel_do" 1 0))
	  ("hufnagel_do3" . ("clefs-hufnagel_do" 3 0))
	  ("hufnagel_fa1" . ("clefs-hufnagel_fa" -1 0))
	  ("hufnagel_fa2" . ("clefs-hufnagel_fa" 1 0))
	  ("hufnagel_do_fa" . ("clefs-hufnagel_do_fa" 4 0))
	  ("mensural_c1" . ("clefs-mensural_c" -2 0))
	  ("mensural_c2" . ("clefs-mensural_c" 0 0))
	  ("mensural_c3" . ("clefs-mensural_c" 2 0))
	  ("mensural_c4" . ("clefs-mensural_c" 4 0))
	  ("mensural_f" . ("clefs-mensural_f" 2 0))
	  ("mensural_g" . ("clefs-mensural_g" -2 0))
	  ("neo_mensural_c1" . ("clefs-neo_mensural_c" -4 0))
	  ("neo_mensural_c2" . ("clefs-neo_mensural_c" -2 0))
	  ("neo_mensural_c3" . ("clefs-neo_mensural_c" 0 0))
	  ("neo_mensural_c4" . ("clefs-neo_mensural_c" 2 0))
	  ("petrucci_c1" . ("clefs-petrucci_c1" -4 0))
	  ("petrucci_c2" . ("clefs-petrucci_c2" -2 0))
	  ("petrucci_c3" . ("clefs-petrucci_c3" 0 0))
	  ("petrucci_c4" . ("clefs-petrucci_c4" 2 0))
	  ("petrucci_c5" . ("clefs-petrucci_c5" 4 0))
	  ("petrucci_f" . ("clefs-petrucci_f" 2 0))
	  ("petrucci_g" . ("clefs-petrucci_g" -2 0))
	)
)


;; "an alist mapping GLYPHNAME to the position of the central C for that symbol"
(define c0-pitch-alist
  '(("clefs-G" . -4)
    ("clefs-C" . 0)
    ("clefs-F" . 4)
    ("clefs-percussion" . 0)
    ("clefs-tab" . 0 )
    ("clefs-vaticana_do" . 0)
    ("clefs-vaticana_fa" . 4)
    ("clefs-medicaea_do" . 0)
    ("clefs-medicaea_fa" . 4)
    ("clefs-hufnagel_do" . 0)
    ("clefs-hufnagel_fa" . 4)
    ("clefs-hufnagel_do_fa" . 0)
    ("clefs-mensural_c" . 0)
    ("clefs-mensural_f" . 4)
    ("clefs-mensural_g" . -4)
    ("clefs-neo_mensural_c" . 0)
    ("clefs-petrucci_c1" . 0)
    ("clefs-petrucci_c2" . 0)
    ("clefs-petrucci_c3" . 0)
    ("clefs-petrucci_c4" . 0)
    ("clefs-petrucci_c5" . 0)
    ("clefs-petrucci_f" . 4)
    ("clefs-petrucci_g" . -4)
  )
)

(define-public (make-clef-set cl)
  "Generate the clef setting commands for a clef with name CL."
  (define (make-prop-set props)
    (let*
	(
	 (m     (make-music-by-name 'PropertySet))
	 )

      (map (lambda (x) (ly-set-mus-property! m (car x) (cdr x))) props)
      m
    ))
    
  (let ((e '())
	(c0 0)
	(oct 0)
	(l (string-length cl))
	)

    ;; ugh. cleanme
    (if (equal? "8" (substring cl (- l 1) l))
	(begin
	(if (equal? "^" (substring cl (- l 2) (- l 1)))
	    (set! oct -7)
	    (set! oct 7))
	
	(set! cl (substring cl 0 (- l 2)))))


    (set! e  (assoc cl supported-clefs))
    
    (if (pair? e)
	(let* 
	    (
	     (musics (map make-prop-set  
	  
			  `(((symbol . clefGlyph)
			     (value . ,(cadr e))
			     )
			    ((symbol . centralCPosition)
			     (value . ,(+ oct (caddr e) (cdr  (assoc  (cadr e) c0-pitch-alist))))
			     )
			    ((symbol . clefPosition)
			     (value . ,(caddr e))
			     )
			    ((symbol . clefOctavation)
			     (value . ,(- oct))
			     )
			    )))
	     (seq (make-music-by-name 'SequentialMusic))
	     (csp (make-music-by-name 'ContextSpeccedMusic))
	     )

	  (ly-set-mus-property! seq 'elements musics)
	  (ly-set-mus-property! csp 'element seq)
	  (ly-set-mus-property! csp 'context-type "Staff")

	  csp
	  )
	(begin
	  (ly-warn (format "Unknown clef type `~a'
See scm/lily.scm for supported clefs"))
	  (make-music-by-name 'Music)
	  
	)
    )))
