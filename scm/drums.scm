;;;; drum-"hack". See input/tricks/drums.ly and ly/drumpitch.ly
;;;; 2001/03/25 Rune Zedeler <rune@zedeler.dk>

;;;; changed eval to primitive-eval for guile 1.4/1.4.1 compatibility --jcn

(define (make-articulation-script x) 
     (let* (  (m (ly-make-music "Articulation_req"))
           )
     (ly-set-mus-property m 'articulation-type x)
     m
     )
 )

;; adds the articulation script x to m if x is not #f.
(define (add-articulation-script m x)
  (if x
   (if (and x (equal? (ly-music-name m) "Request_chord"))
     (ly-set-mus-property m 'elements
       (cons (make-articulation-script x) (ly-get-mus-property m 'elements))
     )
     (let* ( (es (ly-get-mus-property m 'elements))
            (e (ly-get-mus-property m 'element)) )
       (map (lambda (y) (add-articulation-script y x)) es)
       (if (music? e)
         (add-articulation-script e x))
     )
   )
  )
  m
 )

(define (make-head-type-elem t)
   (let* ( (m (ly-make-music "Music"))
         )
     (ly-set-mus-property m 'iterator-ctor Push_property_iterator::constructor)
     (ly-set-mus-property m 'symbol 'NoteHead)
     (ly-set-mus-property m 'grob-property 'style)
     (ly-set-mus-property m 'grob-value t)
     m
   )
 )

(define (make-head-type t)
   (let* ( (m (ly-make-music "Context_specced_music"))
           (e (make-head-type-elem t))
         )
     (ly-set-mus-property m 'element e)
     (ly-set-mus-property m 'context-type "Thread")
     m
   )
 )

(define (make-thread-context thread-name element)
   (let* ( (m (ly-make-music "Context_specced_music")))
     (ly-set-mus-property m 'element element)
     (ly-set-mus-property m 'context-type "Thread")
     (ly-set-mus-property m 'context-id (symbol->string thread-name))
     m
   )
 )

;; makes a sequential-music of thread-context, head-change and note
(define (make-drum-head kit req-ch )
  (let ((es (ly-get-mus-property req-ch 'elements)))
   (if (equal? es '())
    req-ch
    (let* ((fe (car es))
           (oldp (ly-get-mus-property fe 'pitch))
	  )
      (if (not (pitch? oldp))
       req-ch
       (let* ((pap ((pitch->paper kit) oldp ))
	      (style (car pap))
	      (script (cadr pap))
	      (pitch (caddr pap))
	      (ht (make-head-type style))
	      (seq (make-sequential-music (list ht req-ch)))
             )
         (add-articulation-script req-ch script)
         (ly-set-mus-property fe 'pitch pitch)
         (set! req-ch (make-thread-context style seq))
	 req-ch
       )
      )
    )
   )
  )
 )

;; whoa, hadn't head of "assoc" when I made this :)
(define ((pitch->paper kit) p)
   (let p2p ((pitches drum-pitch-names))
     (cond ((eq? pitches '())     
	      (begin
	       (display p) ;; UGH. FIXME. pitch->string ???
	       (ly-warn " unknown drumpitch.")
	       (cdar (primitive-eval kit))
	   ))
         ((eq? p (caddr (car pitches))) ((name->paper kit) (caar pitches)) )
	 (else                          (p2p (cdr pitches) ) )
     )
   )
 )
(define ((name->paper kit) n)
   (let n2p ((pitches (primitive-eval kit)))
     (cond ((eq? pitches '())
	      (begin
	       (ly-warn (string-append "Kit `" (symbol->string kit) "' doesn't contain drum `" n
				       "'\nSee lily/drumpitch.ly for supported drums."))
	       (cdar (primitive-eval kit))
	     ))
           ((eq? n (caar pitches))  (cdar pitches) )
	   (else                    (n2p (cdr pitches) ) )
     )
   )
 )


;; converts a midi-pitched (ly/drumpitch.ly) file to paper output.
(define ((drums->paper kit) music)
  (begin
   (if (equal? (ly-music-name music) "Request_chord")
    (set! music (make-drum-head kit music))
    (let* ((es (ly-get-mus-property music 'elements))
           (e (ly-get-mus-property music 'element))
           (p (ly-get-mus-property music 'pitch))
           (body (ly-get-mus-property music 'body))
           (alts (ly-get-mus-property music 'alternatives)))

      (if (pair? es)
	(ly-set-mus-property music 'elements (map (drums->paper kit) es) )
      )

      (if (music? alts)
        (ly-set-mus-property
         music 'alternatives
         ((drums->paper kit) alts)))

      (if (music? body)
        (ly-set-mus-property
         music 'body
         ((drums->paper kit) body)))

      (if (music? e)
        (begin
          (ly-set-mus-property
           music 'element
           ((drums->paper kit) e))
        )
      )
    )
   )
   music
  )
 )
