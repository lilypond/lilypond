;;;; drum-"hack". See input/tricks/drums.ly and ly/drumpitch.ly
;;;; 2001/03/25 Rune Zedeler <rune@zedeler.dk>

;;;; changed eval to primitive-eval for guile 1.4/1.4.1 compatibility --jcn


;; ugh. Should make separate module?
(define-public drum-pitch-names `(
	(acousticbassdrum bda	,(ly:make-pitch -3 6 0 ))
	(bassdrum	  bd	,(ly:make-pitch -2 0 0 ))
	(hisidestick	  ssh	,(ly:make-pitch -3 6 2))
	(sidestick	  ss	,(ly:make-pitch -2 0 1))
	(losidestick	  ssl	,(ly:make-pitch -2 1 -1))
	(acousticsnare	  sna	,(ly:make-pitch -2 1 0))
	(snare		  sn	,(ly:make-pitch -2 2 -2))
	(handclap	  hc	,(ly:make-pitch -2 1 1))
	(electricsnare	  sne	,(ly:make-pitch -2 2 0))
	(lowfloortom	  tomfl	,(ly:make-pitch -2 3 0))
	(closedhihat	  hhc	,(ly:make-pitch -2 3 1))
	(hihat		  hh	,(ly:make-pitch -2 4 -1))
	(highfloortom	  tomfh	,(ly:make-pitch -2 4 0))
	(pedalhihat	  hhp	,(ly:make-pitch -2 4 1))
	(lowtom		  toml	,(ly:make-pitch -2 5 0))
	(openhihat	  hho	,(ly:make-pitch -2 5 1))
	(halfopenhihat	  hhho	,(ly:make-pitch -2 5 1))
	(lowmidtom	  tomml	,(ly:make-pitch -2 6 0))
	(himidtom	  tommh	,(ly:make-pitch -1 0 0))
	(crashcymbala	  cymca	,(ly:make-pitch -1 0 1))
	(crashcymbal	  cymc	,(ly:make-pitch -1 1 -1))
	(hightom	  tomh	,(ly:make-pitch -1 1 0))
	(ridecymbala	  cymra	,(ly:make-pitch -1 1 1))
	(ridecymbal	  cymr	,(ly:make-pitch -1 2 -1))
	(chinesecymbal	  cymch	,(ly:make-pitch -1 2 0))
	(ridebell	  rb	,(ly:make-pitch -1 3 0))
	(tambourine	  tamb	,(ly:make-pitch -1 3 1))
	(splashcymbal	  cyms	,(ly:make-pitch -1 4 0))
	(cowbell	  cb	,(ly:make-pitch -1 4 1))
	(crashcymbalb	  cymcb	,(ly:make-pitch -1 5 0))
	(vibraslap	  vibs	,(ly:make-pitch -1 5 1))
	(ridecymbalb	  cymrb	,(ly:make-pitch -1 6 0))
	(mutehibongo      bohm  ,(ly:make-pitch -1 6 1))
	(hibongo	  boh	,(ly:make-pitch 0 0 0))
	(openhibongo      boho  ,(ly:make-pitch 0 1 -2))
	(mutelobongo      bolm  ,(ly:make-pitch -1 6 2))
	(lobongo	  bol	,(ly:make-pitch 0 0 1))
	(openlobongo      bolo  ,(ly:make-pitch 0 1 -1))
	(mutehiconga	  cghm	,(ly:make-pitch 0 1 0))
	(muteloconga	  cglm	,(ly:make-pitch 0 2 -2))
	(openhiconga	  cgho	,(ly:make-pitch 0 1 1))
	(hiconga	  cgh	,(ly:make-pitch 0 2 -1))
	(openloconga      cglo  ,(ly:make-pitch 0 1 2))
	(loconga	  cgl	,(ly:make-pitch 0 2 0))
	(hitimbale	  timh	,(ly:make-pitch 0 3 0))
	(lotimbale	  timl	,(ly:make-pitch 0 3 1))
	(hiagogo	  agh	,(ly:make-pitch 0 4 0))
	(loagogo	  agl	,(ly:make-pitch 0 4 1))
	(cabasa		  cab	,(ly:make-pitch 0 5 0))
	(maracas	  mar	,(ly:make-pitch 0 5 1))
	(shortwhistle	  whs	,(ly:make-pitch 0 6 0))
	(longwhistle	  whl	,(ly:make-pitch 1 0 0))
	(shortguiro	  guis	,(ly:make-pitch 1 0 1))
	(longguiro	  guil	,(ly:make-pitch 1 1 0))
	(guiro		  gui	,(ly:make-pitch 1 0 2))
	(claves		  cl	,(ly:make-pitch 1 1 1))
	(hiwoodblock	  wbh	,(ly:make-pitch 1 2 0))
	(lowoodblock	  wbl	,(ly:make-pitch 1 3 0))
	(mutecuica	  cuim	,(ly:make-pitch 1 3 1))
	(opencuica	  cuio	,(ly:make-pitch 1 4 0))
	(mutetriangle	  trim	,(ly:make-pitch 1 4 1))
	(triangle	  tri	,(ly:make-pitch 1 4 2))
	(opentriangle	  trio	,(ly:make-pitch 1 5 0))
	;; "transposing" pitches:
	(oneup		  ua	,(ly:make-pitch 0 1 0))
	(twoup		  ub	,(ly:make-pitch 0 2 0))
	(threeup	  uc	,(ly:make-pitch 0 3 0))
	(fourup		  ud	,(ly:make-pitch 0 4 0))
	(fiveup		  ue	,(ly:make-pitch 0 5 0))
	(onedown	  da	,(ly:make-pitch -1 6 0))
	(twodown	  db	,(ly:make-pitch -1 5 0))
	(threedown	  dc	,(ly:make-pitch -1 4 0))
	(fourdown	  dd	,(ly:make-pitch -1 3 0))
	(fivedown	  de	,(ly:make-pitch -1 2 0))
))

(define-public drums `(
	(acousticbassdrum default 	#f	  ,(ly:make-pitch -1 4 0))
	(bassdrum	  default 	#f	  ,(ly:make-pitch -1 4 0))
	(sidestick	  cross		#f	  ,(ly:make-pitch 0 1 0))
	(acousticsnare	  default 	#f	  ,(ly:make-pitch 0 1 0))
	(snare		  default 	#f	  ,(ly:make-pitch 0 1 0))
	(handclap	  triangle	#f	  ,(ly:make-pitch 0 1 0))
	(electricsnare	  default 	#f	  ,(ly:make-pitch 0 1 0))
	(lowfloortom	  default 	#f	  ,(ly:make-pitch -1 3 0))
	(closedhihat	  cross		"stopped" ,(ly:make-pitch 0 3 0))
	(hihat		  cross		#f	  ,(ly:make-pitch 0 3 0))
	(highfloortom	  default 	#f	  ,(ly:make-pitch -1 5 0))
	(pedalhihat	  cross		#f	  ,(ly:make-pitch -1 2 0))
	(lowtom		  default 	#f	  ,(ly:make-pitch -1 6 0))
	(openhihat	  cross		"open"	  ,(ly:make-pitch 0 3 0))
	(halfopenhihat	  xcircle	#f	  ,(ly:make-pitch 0 3 0))
	(lowmidtom	  default 	#f	  ,(ly:make-pitch 0 0 0))
	(himidtom	  default 	#f	  ,(ly:make-pitch 0 2 0))
	(crashcymbala	  xcircle 	#f	  ,(ly:make-pitch 0 5 0))
	(crashcymbal	  xcircle 	#f	  ,(ly:make-pitch 0 5 0))
	(hightom	  default 	#f	  ,(ly:make-pitch 0 4 0))
	(ridecymbala	  cross		#f	  ,(ly:make-pitch 0 5 0))
	(ridecymbal	  cross		#f	  ,(ly:make-pitch 0 5 0))
	(chinesecymbal	  mensural 	#f	  ,(ly:make-pitch 0 5 0))
	(ridebell	  default	#f	  ,(ly:make-pitch 0 5 0))
	(splashcymbal	  diamond 	#f	  ,(ly:make-pitch 0 5 0))
	(cowbell	  triangle	#f	  ,(ly:make-pitch 0 5 0))
	(crashcymbalb	  cross		#f	  ,(ly:make-pitch 0 5 0))
	(vibraslap	  diamond 	#f	  ,(ly:make-pitch 0 4 0))
	(ridecymbalb	  cross		#f	  ,(ly:make-pitch 0 5 0))
 ))

(define-public timbales `(
	(losidestick	  cross		#f	  ,(ly:make-pitch -1 6 0))
	(lotimbale	  default	#f	  ,(ly:make-pitch -1 6 0))
	(cowbell	  triangle	#f	  ,(ly:make-pitch 0 2 0))
	(hisidestick	  cross		#f	  ,(ly:make-pitch 0 1 0))
	(hitimbale	  default	#f	  ,(ly:make-pitch 0 1 0))
 ))

(define-public congas `(
	(losidestick	  cross		#f	  ,(ly:make-pitch -1 6 0))
	(loconga	  default	#f	  ,(ly:make-pitch -1 6 0))
	(openloconga      default       "open"    ,(ly:make-pitch -1 6 0))
	(muteloconga      default       "stopped" ,(ly:make-pitch -1 6 0))
	(hisidestick	  cross		#f	  ,(ly:make-pitch 0 1 0))
	(hiconga	  default	#f	  ,(ly:make-pitch 0 1 0))
	(openhiconga      default       "open"    ,(ly:make-pitch 0 1 0))
        (mutehiconga      default       "stopped" ,(ly:make-pitch 0 1 0))
  
 ))

(define-public bongos `(
	(losidestick	  cross		#f	  ,(ly:make-pitch -1 6 0))
	(lobongo	  default	#f	  ,(ly:make-pitch -1 6 0))
	(openlobongo      default       "open"    ,(ly:make-pitch -1 6 0))
	(mutelobongo      default       "stopped" ,(ly:make-pitch -1 6 0))
	(hisidestick	  cross		#f	  ,(ly:make-pitch 0 1 0))
	(hibongo	  default	#f	  ,(ly:make-pitch 0 1 0))
	(openhibongo      default       "open"    ,(ly:make-pitch 0 1 0))
	(mutehibongo      default       "stopped" ,(ly:make-pitch 0 1 0))
 ))


(define-public percussion `(
	(opentriangle	  cross		"open"	  ,(ly:make-pitch 0 0 0))
	(mutetriangle	  cross		"stopped" ,(ly:make-pitch 0 0 0))
	(triangle	  cross		#f	  ,(ly:make-pitch 0 0 0))
	(shortguiro	  default	"staccato",(ly:make-pitch 0 0 0))
	(longguiro	  default	"tenuto"  ,(ly:make-pitch 0 0 0))
	(guiro		  default	#f	  ,(ly:make-pitch 0 0 0))
	(cowbell	  triangle	#f	  ,(ly:make-pitch 0 0 0))
	(claves		  default	#f	  ,(ly:make-pitch 0 0 0))
	(tambourine	  default	#f	  ,(ly:make-pitch 0 0 0))
	(cabasa		  cross		#f	  ,(ly:make-pitch 0 0 0))
	(maracas	  default	#f	  ,(ly:make-pitch 0 0 0))
	(handclap	  default	#f	  ,(ly:make-pitch 0 0 0))
 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (make-articulation-script x) 
     (let* ((m (make-music-by-name 'ArticulationEvent)))
     (ly:set-mus-property! m 'articulation-type x)
     m))

;; adds the articulation script x to m if x is not #f.
(define (add-articulation-script m x)
  (if x
   (if (and x (equal? (ly:music-name m) "Request_chord"))
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
   (let* ( (m (make-music-by-name 'Music)))
     (set-mus-properties!
      m
      `((iterator-ctor . ,Push_property_iterator::constructor)
	(symbol . NoteHead)
	(grob-property . style)
	(grob-value . ,t)
	(pop-first  . #t)))
      m

   )
 )

(define (make-head-type t)
  (context-spec-music (make-head-type-elem t) "Thread"))

(define (make-thread-context thread-name element)
  (context-spec-music element "Thread" thread-name))

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
	       (ly:warn " unknown drumpitch.")
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
	       (ly:warn (string-append "Kit `" (symbol->string kit) "' doesn't contain drum `" n
				       "'\nSee lily/drumpitch.ly for supported drums."))
	       (cdar (primitive-eval kit))
	     ))
           ((eq? n (caar pitches))  (cdar pitches) )
	   (else                    (n2p (cdr pitches) ) )
     )
   )
 )


;; converts a midi-pitched (ly/drumpitch.ly) file to paper output.
(define-public ((drums->paper kit) music)
  (begin
   (if (equal? (ly:music-name music) "Request_chord")
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
        )
      )
    )
   )
   music
  )
 )
