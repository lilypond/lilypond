;;;; drum-"hack". See input/regression/drums.ly and ly/drumpitch-init.ly
;;;; 2001/03/25 Rune Zedeler <rune@zedeler.dk>

;;;; changed eval to primitive-eval for guile 1.4/1.4.1 compatibility --jcn

;; TODO: the design of this hack should be rethought.


;; ugh. Should make separate module?
(define-public drum-pitch-names `(
	(acousticbassdrum bda	,(ly:make-pitch -3 6 NATURAL))
	(bassdrum	  bd	,(ly:make-pitch -2 0 NATURAL))
	(hisidestick	  ssh	,(ly:make-pitch -3 6 DOUBLE-SHARP))
	(sidestick	  ss	,(ly:make-pitch -2 0 SHARP))
	(losidestick	  ssl	,(ly:make-pitch -2 1 FLAT))
	(acousticsnare	  sna	,(ly:make-pitch -2 1 NATURAL))
	(snare		  sn	,(ly:make-pitch -2 2 DOUBLE-FLAT))
	(handclap	  hc	,(ly:make-pitch -2 1 SHARP))
	(electricsnare	  sne	,(ly:make-pitch -2 2 NATURAL))
	(lowfloortom	  tomfl	,(ly:make-pitch -2 3 NATURAL))
	(closedhihat	  hhc	,(ly:make-pitch -2 3 SHARP))
	(hihat		  hh	,(ly:make-pitch -2 4 FLAT))
	(highfloortom	  tomfh	,(ly:make-pitch -2 4 NATURAL))
	(pedalhihat	  hhp	,(ly:make-pitch -2 4 SHARP))
	(lowtom		  toml	,(ly:make-pitch -2 5 NATURAL))
	(openhihat	  hho	,(ly:make-pitch -2 5 SHARP))
	(halfopenhihat	  hhho	,(ly:make-pitch -2 5 SHARP))
	(lowmidtom	  tomml	,(ly:make-pitch -2 6 NATURAL))
	(himidtom	  tommh	,(ly:make-pitch -1 0 NATURAL))
	(crashcymbala	  cymca	,(ly:make-pitch -1 0 SHARP))
	(crashcymbal	  cymc	,(ly:make-pitch -1 1 FLAT))
	(hightom	  tomh	,(ly:make-pitch -1 1 NATURAL))
	(ridecymbala	  cymra	,(ly:make-pitch -1 1 SHARP))
	(ridecymbal	  cymr	,(ly:make-pitch -1 2 FLAT))
	(chinesecymbal	  cymch	,(ly:make-pitch -1 2 NATURAL))
	(ridebell	  rb	,(ly:make-pitch -1 3 NATURAL))
	(tambourine	  tamb	,(ly:make-pitch -1 3 SHARP))
	(splashcymbal	  cyms	,(ly:make-pitch -1 4 NATURAL))
	(cowbell	  cb	,(ly:make-pitch -1 4 SHARP))
	(crashcymbalb	  cymcb	,(ly:make-pitch -1 5 NATURAL))
	(vibraslap	  vibs	,(ly:make-pitch -1 5 SHARP))
	(ridecymbalb	  cymrb	,(ly:make-pitch -1 6 NATURAL))
	(mutehibongo      bohm  ,(ly:make-pitch -1 6 SHARP))
	(hibongo	  boh	,(ly:make-pitch 0 0 NATURAL))
	(openhibongo      boho  ,(ly:make-pitch 0 1 DOUBLE-FLAT))
	(mutelobongo      bolm  ,(ly:make-pitch -1 6 DOUBLE-SHARP))
	(lobongo	  bol	,(ly:make-pitch 0 0 SHARP))
	(openlobongo      bolo  ,(ly:make-pitch 0 1 FLAT))
	(mutehiconga	  cghm	,(ly:make-pitch 0 1 NATURAL))
	(muteloconga	  cglm	,(ly:make-pitch 0 2 DOUBLE-FLAT))
	(openhiconga	  cgho	,(ly:make-pitch 0 1 SHARP))
	(hiconga	  cgh	,(ly:make-pitch 0 2 FLAT))
	(openloconga      cglo  ,(ly:make-pitch 0 1 DOUBLE-SHARP))
	(loconga	  cgl	,(ly:make-pitch 0 2 NATURAL))
	(hitimbale	  timh	,(ly:make-pitch 0 3 NATURAL))
	(lotimbale	  timl	,(ly:make-pitch 0 3 SHARP))
	(hiagogo	  agh	,(ly:make-pitch 0 4 NATURAL))
	(loagogo	  agl	,(ly:make-pitch 0 4 SHARP))
	(cabasa		  cab	,(ly:make-pitch 0 5 NATURAL))
	(maracas	  mar	,(ly:make-pitch 0 5 SHARP))
	(shortwhistle	  whs	,(ly:make-pitch 0 6 NATURAL))
	(longwhistle	  whl	,(ly:make-pitch 1 0 NATURAL))
	(shortguiro	  guis	,(ly:make-pitch 1 0 SHARP))
	(longguiro	  guil	,(ly:make-pitch 1 1 NATURAL))
	(guiro		  gui	,(ly:make-pitch 1 0 DOUBLE-SHARP))
	(claves		  cl	,(ly:make-pitch 1 1 SHARP))
	(hiwoodblock	  wbh	,(ly:make-pitch 1 2 NATURAL))
	(lowoodblock	  wbl	,(ly:make-pitch 1 3 NATURAL))
	(mutecuica	  cuim	,(ly:make-pitch 1 3 SHARP))
	(opencuica	  cuio	,(ly:make-pitch 1 4 NATURAL))
	(mutetriangle	  trim	,(ly:make-pitch 1 4 SHARP))
	(triangle	  tri	,(ly:make-pitch 1 4 DOUBLE-SHARP))
	(opentriangle	  trio	,(ly:make-pitch 1 5 NATURAL))
	;; "transposing" pitches:
	(oneup		  ua	,(ly:make-pitch 0 1 NATURAL))
	(twoup		  ub	,(ly:make-pitch 0 2 NATURAL))
	(threeup	  uc	,(ly:make-pitch 0 3 NATURAL))
	(fourup		  ud	,(ly:make-pitch 0 4 NATURAL))
	(fiveup		  ue	,(ly:make-pitch 0 5 NATURAL))
	(onedown	  da	,(ly:make-pitch -1 6 NATURAL))
	(twodown	  db	,(ly:make-pitch -1 5 NATURAL))
	(threedown	  dc	,(ly:make-pitch -1 4 NATURAL))
	(fourdown	  dd	,(ly:make-pitch -1 3 NATURAL))
	(fivedown	  de	,(ly:make-pitch -1 2 NATURAL))
))

;;
;; all settings for percussive instruments.
;; public so people can add their own stuff.
;;

(define-public
  percussive-instrument-init-settings
  `((drums
    . (
	(acousticbassdrum default	#f	  ,(ly:make-pitch -1 4 NATURAL))
	(bassdrum	  default 	#f	  ,(ly:make-pitch -1 4 NATURAL))
	(sidestick	  cross		#f	  ,(ly:make-pitch 0 1 NATURAL))
	(acousticsnare	  default 	#f	  ,(ly:make-pitch 0 1 NATURAL))
	(snare		  default 	#f	  ,(ly:make-pitch 0 1 NATURAL))
	(handclap	  triangle	#f	  ,(ly:make-pitch 0 1 NATURAL))
	(electricsnare	  default 	#f	  ,(ly:make-pitch 0 1 NATURAL))
	(lowfloortom	  default 	#f	  ,(ly:make-pitch -1 3 NATURAL))
	(closedhihat	  cross		"stopped" ,(ly:make-pitch 0 3 NATURAL))
	(hihat		  cross		#f	  ,(ly:make-pitch 0 3 NATURAL))
	(highfloortom	  default 	#f	  ,(ly:make-pitch -1 5 NATURAL))
	(pedalhihat	  cross		#f	  ,(ly:make-pitch -1 2 NATURAL))
	(lowtom		  default 	#f	  ,(ly:make-pitch -1 6 NATURAL))
	(openhihat	  cross		"open"	  ,(ly:make-pitch 0 3 NATURAL))
	(halfopenhihat	  xcircle	#f	  ,(ly:make-pitch 0 3 NATURAL))
	(lowmidtom	  default 	#f	  ,(ly:make-pitch 0 0 NATURAL))
	(himidtom	  default 	#f	  ,(ly:make-pitch 0 2 NATURAL))
	(crashcymbala	  xcircle 	#f	  ,(ly:make-pitch 0 5 NATURAL))
	(crashcymbal	  xcircle 	#f	  ,(ly:make-pitch 0 5 NATURAL))
	(hightom	  default 	#f	  ,(ly:make-pitch 0 4 NATURAL))
	(ridecymbala	  cross		#f	  ,(ly:make-pitch 0 5 NATURAL))
	(ridecymbal	  cross		#f	  ,(ly:make-pitch 0 5 NATURAL))
	(chinesecymbal	  mensural 	#f	  ,(ly:make-pitch 0 5 NATURAL))
	(ridebell	  default	#f	  ,(ly:make-pitch 0 5 NATURAL))
	(splashcymbal	  diamond 	#f	  ,(ly:make-pitch 0 5 NATURAL))
	(cowbell	  triangle	#f	  ,(ly:make-pitch 0 5 NATURAL))
	(crashcymbalb	  cross		#f	  ,(ly:make-pitch 0 5 NATURAL))
	(vibraslap	  diamond 	#f	  ,(ly:make-pitch 0 4 NATURAL))
	(ridecymbalb	  cross		#f	  ,(ly:make-pitch 0 5 NATURAL))
      ))

  (timbales
   . (
	(losidestick	  cross		#f	  ,(ly:make-pitch -1 6 NATURAL))
	(lotimbale	  default	#f	  ,(ly:make-pitch -1 6 NATURAL))
	(cowbell	  triangle	#f	  ,(ly:make-pitch 0 2 NATURAL))
	(hisidestick	  cross		#f	  ,(ly:make-pitch 0 1 NATURAL))
	(hitimbale	  default	#f	  ,(ly:make-pitch 0 1 NATURAL))
      ))

  (congas
   . (
	(losidestick	  cross		#f	  ,(ly:make-pitch -1 6 NATURAL))
	(loconga	  default	#f	  ,(ly:make-pitch -1 6 NATURAL))
	(openloconga      default       "open"    ,(ly:make-pitch -1 6 NATURAL))
	(muteloconga      default       "stopped" ,(ly:make-pitch -1 6 NATURAL))
	(hisidestick	  cross		#f	  ,(ly:make-pitch 0 1 NATURAL))
	(hiconga	  default	#f	  ,(ly:make-pitch 0 1 NATURAL))
	(openhiconga      default       "open"    ,(ly:make-pitch 0 1 NATURAL))
	(mutehiconga      default       "stopped" ,(ly:make-pitch 0 1 NATURAL))
      ))

  (bongos
    . (
 	(losidestick	  cross		#f	  ,(ly:make-pitch -1 6 NATURAL))
	(lobongo	  default	#f	  ,(ly:make-pitch -1 6 NATURAL))
	(openlobongo      default       "open"    ,(ly:make-pitch -1 6 NATURAL))
	(mutelobongo      default       "stopped" ,(ly:make-pitch -1 6 NATURAL))
	(hisidestick	  cross		#f	  ,(ly:make-pitch 0 1 NATURAL))
	(hibongo	  default	#f	  ,(ly:make-pitch 0 1 NATURAL))
	(openhibongo      default       "open"    ,(ly:make-pitch 0 1 NATURAL))
	(mutehibongo      default       "stopped" ,(ly:make-pitch 0 1 NATURAL))
      ))


  (percussion
   . (
	(opentriangle	  cross		"open"	  ,(ly:make-pitch 0 0 NATURAL))
	(mutetriangle	  cross		"stopped" ,(ly:make-pitch 0 0 NATURAL))
	(triangle	  cross		#f	  ,(ly:make-pitch 0 0 NATURAL))
	(shortguiro	  default	"staccato",(ly:make-pitch 0 0 NATURAL))
	(longguiro	  default	"tenuto"  ,(ly:make-pitch 0 0 NATURAL))
	(guiro		  default	#f	  ,(ly:make-pitch 0 0 NATURAL))
	(cowbell	  triangle	#f	  ,(ly:make-pitch 0 0 NATURAL))
	(claves		  default	#f	  ,(ly:make-pitch 0 0 NATURAL))
	(tambourine	  default	#f	  ,(ly:make-pitch 0 0 NATURAL))
	(cabasa		  cross		#f	  ,(ly:make-pitch 0 0 NATURAL))
	(maracas	  default	#f	  ,(ly:make-pitch 0 0 NATURAL))
	(handclap	  default	#f	  ,(ly:make-pitch 0 0 NATURAL))
      ))
  ))


(define percussive-instrument-settings percussive-instrument-init-settings)

;; don't use assoc-set!, since this will overwrite Scheme defaults, and leak
;; into other files.
(define-public (set-drum-kit kit value)
  (set! percussive-instrument-settings
	(acons kit value  percussive-instrument-settings)))

(define-public (reset-drum-kit)
  (set! percussive-instrument-settings percussive-instrument-init-settings))

(define-public (get-drum-kit kit)
  (assoc-get-default kit percussive-instrument-settings '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (make-articulation-script x) 
     (let* ((m (make-music-by-name 'ArticulationEvent)))
     (ly:set-mus-property! m 'articulation-type x)
     m))

;; adds the articulation script x to m if x is not #f.
(define (add-articulation-script m x)
  (if x
   (if (and x (equal? (ly:get-mus-property m 'name) 'EventChord))
     (ly:set-mus-property! m 'elements
       (cons (make-articulation-script x) (ly:get-mus-property m 'elements))
     )
     (let* ( (es (ly:get-mus-property m 'elements))
            (e (ly:get-mus-property m 'element)) )
       (map (lambda (y) (add-articulation-script y x)) es)
       (if (ly:music? e)
         (add-articulation-script e x))
     )
   )
  )
  m
 )

(define (make-head-type-elem t)
   (let* ( (m (make-music-by-name 'OverrideProperty)))
     (set-mus-properties!
      m
      `((symbol . NoteHead)
	(grob-property . style)
	(grob-value . ,t)
	(pop-first  . #t)))
      m

   )
 )

(define (make-head-type t)
  (context-spec-music (make-head-type-elem t) 'Thread))

(define (make-thread-context thread-name element)
  (context-spec-music element 'Thread thread-name))

;; makes a sequential-music of thread-context, head-change and note
(define (make-drum-head kit req-ch )
  (let ((es (ly:get-mus-property req-ch 'elements)))
   (if (equal? es '())
    req-ch
    (let* ((fe (car es))
           (oldp (ly:get-mus-property fe 'pitch))
	  )
      (if (not (ly:pitch? oldp))
       req-ch
       (let* ((pap ((pitch->paper kit) oldp ))
	      (style (car pap))
	      (script (cadr pap))
	      (pitch (caddr pap))
	      (ht (make-head-type style))
	      (seq (make-sequential-music (list ht req-ch)))
             )
         (add-articulation-script req-ch script)
         (ly:set-mus-property! fe 'pitch pitch)
         (set! req-ch (make-thread-context (symbol->string style) seq))
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
	       (ly:warn " unknown drumpitch.")
	       (cdar (get-drum-kit kit))
	   ))
         ((eq? p (caddr (car pitches))) ((name->paper kit) (caar pitches)) )
	 (else                          (p2p (cdr pitches) ) )
     )
   )
 )

(define ((name->paper kit) n)
   (let n2p ((pitches (get-drum-kit kit)))
     (cond ((eq? pitches '())
	      (begin
	       (ly:warn (string-append "Kit `" (symbol->string kit) "' doesn't contain drum `" n
				       "'\nSee ly/drumpitch-init.ly for supported drums."))
	       (cdar (get-drum-kit kit))
	     ))
           ((eq? n (caar pitches))  (cdar pitches) )
	   (else                    (n2p (cdr pitches) ) )
     )
   )
 )

;;
;; converts a midi-pitched (ly/drumpitch.ly) file to paper output.
;;
(define-public ((drums->paper kit) music)
  (begin
   (if (equal? (ly:get-mus-property music 'name) 'EventChord)
    (set! music (make-drum-head kit music))
    (let* ((es (ly:get-mus-property music 'elements))
           (e (ly:get-mus-property music 'element))
           (p (ly:get-mus-property music 'pitch))
           (body (ly:get-mus-property music 'body))
           (alts (ly:get-mus-property music 'alternatives)))

      (if (pair? es)
	(ly:set-mus-property! music 'elements (map (drums->paper kit) es) )
      )

      (if (ly:music? alts)
        (ly:set-mus-property!
         music 'alternatives
         ((drums->paper kit) alts)))

      (if (ly:music? body)
        (ly:set-mus-property!
         music 'body
         ((drums->paper kit) body)))

      (if (ly:music? e)
        (begin
          (ly:set-mus-property!
           music 'element
           ((drums->paper kit) e))
        ))))
   music
  ))


