;;;; auto-beam.scm -- Auto-beam-engraver settings
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>

;;; specify generic beam begin and end times

;;; format:
;;;
;;;   function shortest-duration-in-beam time-signature
;;;
;;; where
;;;
;;;     function = begin or end
;;;     shortest-duration-in-beam = numerator denominator; e.g.: 1 16
;;;     time-signature = numerator denominator, e.g.: 4 4
;;;
;;; unspecified or wildcard entries for duration or time-signature
;;; are given by * *

;;; maybe do:  '(end shortest-1 16 time-3 4) ?

(define-public default-auto-beam-settings
  `(
    ;; in 3 2 time:
    ;;   end beams each 1 2 note
    ;;   end beams with 16th notes each 1 4 note
    ;;   end beams with 32th notes each 1 8 note

    ((end * * 3 2) . ,(ly:make-moment 1 2))
    ((end * * 3 2) . ,(ly:make-moment 2 2))
    
    ((end 1 16 3 2) . ,(ly:make-moment 1 4))
    ((end 1 16 3 2) . ,(ly:make-moment 1 2))
    ((end 1 16 3 2) . ,(ly:make-moment 3 4))
    ((end 1 16 3 2) . ,(ly:make-moment 5 4))
    
    ((end 1 32 3 2) . ,(ly:make-moment 1 8))
    ((end 1 32 3 2) . ,(ly:make-moment 1 4))
    ((end 1 32 3 2) . ,(ly:make-moment 3 8))
    ((end 1 32 3 2) . ,(ly:make-moment 1 2))
    ((end 1 32 3 2) . ,(ly:make-moment 5 8))
    ((end 1 32 3 2) . ,(ly:make-moment 3 4))
    ((end 1 32 3 2) . ,(ly:make-moment 7 8))
    ((end 1 32 3 2) . ,(ly:make-moment 9 8))
    ((end 1 32 3 2) . ,(ly:make-moment 5 4))
    ((end 1 32 3 2) . ,(ly:make-moment 11 8))

    ((end * * 3 4) . ,(ly:make-moment 3 4))

    ((end 1 16 3 4) . ,(ly:make-moment 1 4))
    ((end 1 16 3 4) . ,(ly:make-moment 1 2))
    
    ((end 1 32 3 4) . ,(ly:make-moment 1 8))
    ((end 1 32 3 4) . ,(ly:make-moment 1 4))
    ((end 1 32 3 4) . ,(ly:make-moment 3 8))
    ((end 1 32 3 4) . ,(ly:make-moment 1 2))
    ((end 1 32 3 4) . ,(ly:make-moment 5 8))

    ((end * * 3 8) . ,(ly:make-moment 3 8))

    ;; in common time:
    ;;   end beams each 1 2 note
    ;;   end beams with 32th notes each 1 8 note
    ;;   end beams with 1 8 triplets each 1 4 note

    ((end * * 4 4) . ,(ly:make-moment 1 2))
    ((end 1 12 4 4) . ,(ly:make-moment 1 4))
    ((end 1 12 4 4) . ,(ly:make-moment 3 4))
    
    ((end 1 16 4 4) . ,(ly:make-moment 1 4))
    ((end 1 16 4 4) . ,(ly:make-moment 3 4))
    
    ((end 1 32 4 4) . ,(ly:make-moment 1 8))
    ((end 1 32 4 4) . ,(ly:make-moment 1 4))
    ((end 1 32 4 4) . ,(ly:make-moment 3 8))
    ((end 1 32 4 4) . ,(ly:make-moment 5 8))
    ((end 1 32 4 4) . ,(ly:make-moment 3 4))
    ((end 1 32 4 4) . ,(ly:make-moment 7 8))

    ((end * * 2 4) . #f) ;; switch-off at-any-beat feature
    ((end * * 2 4) . ,(ly:make-moment 1 4))
    ((end 1 32 2 4) . ,(ly:make-moment 1 8))
    ((end 1 32 2 4) . ,(ly:make-moment 3 8))

    ((end * * 4 8) . #f) ;; switch-off at-any-beat feature
    ((end * * 4 8) . ,(ly:make-moment 1 4))
    ((end 1 32 4 8) . ,(ly:make-moment 1 8))
    ((end 1 32 4 8) . ,(ly:make-moment 3 8))

    ((end * * 4 16) . #f) ;; switch-off at-any-beat feature
    ((end * * 4 16) . ,(ly:make-moment 1 8))

    ((end * * 6 8) . #f) ;; switch-off at-any-beat feature
    ((end * * 6 8) . ,(ly:make-moment 3 8))
    ((end 1 32 6 8) . ,(ly:make-moment 1 8))
    ((end 1 32 6 8) . ,(ly:make-moment 1 4))
    ((end 1 32 6 8) . ,(ly:make-moment 1 2))
    ((end 1 32 6 8) . ,(ly:make-moment 5 8))

    ((end * * 9 8) . #f) ;; switch-off at-any-beat feature
    ((end * * 9 8) . ,(ly:make-moment 3 8))
    ((end * * 9 8) . ,(ly:make-moment 3 4))
    ((end 1 32 9 8) . ,(ly:make-moment 1 8))
    ((end 1 32 9 8) . ,(ly:make-moment 1 4))
    ((end 1 32 9 8) . ,(ly:make-moment 1 2))
    ((end 1 32 9 8) . ,(ly:make-moment 5 8))
    ((end 1 32 9 8) . ,(ly:make-moment 7 8))
    ((end 1 32 9 8) . ,(ly:make-moment 1 1))
    ((end 1 32 9 8) . ,(ly:make-moment 9 8))

    ((end * * 12 8) . #f) ;; switch-off at-every-beat
    ((end * * 12 8) . ,(ly:make-moment 3 8))
    ((end * * 12 8) . ,(ly:make-moment 3 4))
    ((end * * 12 8) . ,(ly:make-moment 9 8))
    ((end * * 12 8) . ,(ly:make-moment 2 1))
    ((end 1 32 12 8) . ,(ly:make-moment 1 8))
    ))

(define (override-property-setting context property setting value)
  "Like the C++ code that executes \\override, but without type
checking. "
  (ly:context-set-property!
   context property
   (cons (cons setting value) (ly:context-property context property))))

(define (revert-property-setting context property setting)
  "Like the C++ code that executes \revert, but without type
checking. "

  (define (revert-member alist entry new)
    "Return ALIST, with ENTRY removed.  ALIST is not modified, instead
a fresh copy of the list-head is made."
    (cond
     ((null? alist) new)
     ((equal? (car alist) entry) (revert-member (cdr alist) entry new))
     (else (revert-member (cdr alist) entry (cons (car alist) new)))))

  (ly:context-set-property!
   context property
   (revert-member (ly:context-property context property) setting '())))

(define-public (override-auto-beam-setting setting num den . rest)
  (ly:export
   (context-spec-music
    (make-apply-context (lambda (c)
			  (override-property-setting
			   c 'autoBeamSettings
			   setting (ly:make-moment num den))))
    (if (and (pair? rest) (symbol? (car rest)))
	(car rest)
	'Voice))))

(define-public (score-override-auto-beam-setting setting num den)
  (override-auto-beam-setting setting num den 'Score))

(define-public (revert-auto-beam-setting setting num den . rest)
  (ly:export
   (context-spec-music
    (make-apply-context (lambda (c)
			  (revert-property-setting
			   c 'autoBeamSettings
			   (cons setting (ly:make-moment num den)))))
    (if (and (pair? rest) (symbol? (car rest)))
	(car rest)
	'Voice))))

;;  Determine end moment for auto beaming (or begin moment, but mostly
;;  0== anywhere).  In order of decreasing priority:
;;
;;  1. end <type>   *     *
;;  2. end   *      *     *
;;  3. end <type> <num> <den>
;;  4. end   *    <num> <den>
;;  5. if 1-4 not specified, begin anywhere, end at time determined by
;;          beatGrouping and beatLength:
;;     if beatGrouping and beatLength are consistent with measureLength,
;;        use beatGrouping to determine end of beams.
;;     if beatGrouping and beatLength are inconsistent with measureLength,
;;        use beatLength to determine end of beams.
;;
;;  Rationale:
;;
;;  [user override]
;;  1. override for specific duration type
;;  2. generic override
;;
;;  [to be defined in config file]
;;  3. exceptions for specific time signature, for specific duration type
;;  4. exceptions for specific time signature
;;  5. easy catch-all rule for non-specified measure types


(define-public (default-auto-beam-check context dir test)
  (define (get name default)
    (let ((value (ly:context-property context name)))
      (if (not (null? value)) value default)))

  (define (ending-moments group-list start-beat beat-length)
  (if (null? group-list)
      '()
      (let ((new-start (+ start-beat (car group-list))))
        (cons (ly:moment-mul (ly:make-moment new-start 1) beat-length)
              (ending-moments (cdr group-list) new-start beat-length)))))

  (define (make-end-settings time ending-list moment-den)
    (if (null? ending-list)
        '()
        (cons (cons (append '(end * *) time)
                    (ly:make-moment (car ending-list) moment-den))
              (make-end-settings time (cdr ending-list) moment-den))))
  
  ;; Don't start auto beams on grace notes
  (if (and (!= (ly:moment-grace-numerator (ly:context-now context)) 0)
	   (= dir START))
      #f
      (let* ((beat-length (get 'beatLength (ly:make-moment 1 4)))
	     (measure-length (get 'measureLength (ly:make-moment 1 1)))
	     (measure-pos (get 'measurePosition ZERO-MOMENT))
             (beat-grouping (get 'beatGrouping '()))
	     (settings (get 'autoBeamSettings '()))
	     (function (list (if (= dir START) 'begin 'end)))
             ;; Calculate implied time signature based on measureLength
             ;; and beatLength for default value in get
	     (num-mom (ly:moment-div measure-length beat-length))
	     (num (inexact->exact
		   (round (/ (ly:moment-main-numerator num-mom)
			     (ly:moment-main-denominator num-mom)))))
	     (den (ly:moment-main-denominator beat-length))
             (time-signature-fraction 
               (get 'timeSignatureFraction (cons num den)))
	     (time (list (car time-signature-fraction)
                         (cdr time-signature-fraction)))
	     (type (list (ly:moment-main-numerator test)
			 (ly:moment-main-denominator test)))
	     (pos (if (>= (ly:moment-main-numerator measure-pos) 0)
		      measure-pos
		      (ly:moment-add measure-length measure-pos)))
             (grouping-moments (ending-moments beat-grouping 0 beat-length))
             ;; Calculate implied measure length from beatGrouping
             ;; and beatLength
	     (grouping-length (if (null? grouping-moments)
                                  ZERO-MOMENT
                                  (list-ref grouping-moments 
                                            (1- (length grouping-moments)))))
             (lst (list
		   ;; Hmm, should junk user-override feature,
		   ;; or split this in user-override and config section?
		   (append function type '(* *))
		   (append function '(* * * *))
		   (append function type time)
		   (append function '(* *) time)))
             (predefined-setting (first-assoc lst settings)))
         (if (or
	     ;; always begin or end beams at beginning/ending of measure
	     (= (ly:moment-main-numerator pos) 0)
	     (first-member (map (lambda (x) (cons x pos)) lst) settings))
	    #t
	    (if (= dir START)
		;; if no entry matches our function + time or type,
		;; start anywhere
		(not predefined-setting)
		;; if entry matches our function + time or type, check moment
		(if predefined-setting
                    (equal? measure-pos (cdr predefined-setting))
                    ;; if measure-length matches grouping-length, use
                    ;; grouping moments, else use beat-length
                    (if (equal? measure-length grouping-length)
		        (member measure-pos grouping-moments)
                        (= (ly:moment-main-denominator
			     (ly:moment-div pos beat-length)) 1))))))))
