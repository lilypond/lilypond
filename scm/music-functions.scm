
(define (denominator-tuplet-formatter mus)
  (number->string (ly-get-mus-property mus 'denominator)))

(define (fraction-tuplet-formatter mus)
  (string-append (number->string (ly-get-mus-property mus 'numerator))
		 ":"
		 (number->string (ly-get-mus-property mus 'denominator))
		 ))

(define (unfold-repeats music)
"
This function replaces all repeats  with unfold repeats. It was 
written by Rune Zedeler. "
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (n  (ly-music-name music)))

    (if (equal? n "Repeated_music")
        (begin
          (ly-set-mus-property
           music 'length Repeated_music::unfolded_music_length)
          (ly-set-mus-property
           music 'iterator-ctor Unfolded_repeat_iterator::constructor)))

    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map unfold-repeats es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (unfold-repeats e)))


    music))

(define  (pitchify-scripts music)
  "Copy the pitch fields of the Note_requests into  Text_script_requests, to aid
Fingering_engraver."
  (define (find-note musics)
    (filter-list (lambda (m) (equal? (ly-music-name m) "Note_req")) musics)
    )
  (define (find-scripts musics)
    (filter-list (lambda (m) (equal? (ly-music-name m) "Text_script_req")) musics))

  (let* (
	 (e (ly-get-mus-property music 'element))
	 (es (ly-get-mus-property music 'elements))
	 (notes (find-note es))
	 (pitch (if (pair? notes) (ly-get-mus-property (car  notes) 'pitch) #f))
	 )

    (if pitch
	(map (lambda (x) (ly-set-mus-property x 'pitch pitch)) (find-scripts es))
	)
	
    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map pitchify-scripts es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (pitchify-scripts e)))

    music))
