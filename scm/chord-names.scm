;; note-name: (note . accidental)
;; list:  (list-of-pitches . (modifier-string . addition-subtraction-string))

;; if a complete chord is found, use name
;; if a chord's base triad is found (c e g), use name

(define note-names-alist '())
(set! note-names-alist
      (append 
      '(
	; use these for German naming
	;((6 . 0) . ("H" ""))
	;((6 . -1) . ("B" ("feta-1" . "")))

	; C-p/C-r current feta chars for sharp/flat
	; don't use them: ly2dvi breaks (inputenc package)
	;((0 . 1) . ("C" ("feta-1" . "")))
	;((0 . -1) . ("C" ("feta-1" . "")))
	)
      note-names-alist))

(define (pitch->note-name pitch)
  (cons (cadr pitch) (caddr pitch)))
  
(define (user-pitch-name pitch)
  (let ((entry (assoc (pitch->note-name pitch) note-names-alist)))
       (if entry
	   (cdr entry))))

(define chord-names-alist '())
(set! chord-names-alist
      (append 
      '(
	; C iso C.no3.no5
	(((0 . 0)) . (#f . #f))
	; C iso C.no5
	(((0 . 0) (2 . 0)) . (#f . #f))
	; Cm iso Cm.no5
	(((0 . 0) (2 . -1)) . ("m" . #f))
	; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ("dim" . #f))
	; Co iso Cm5-7-
	; urg
        ; (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" . ("feta-1" . ".")))
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (#f . ("script" . "o")))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1)) . ("dim" . ("script" . "9")))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1)) . ("dim" . ("script" . "11")))
	)
      chord-names-alist))

(define (user-chord-name chord)
  (let ((entry (assoc (map (lambda (x) (pitch->note-name x)) chord)
		      chord-names-alist)))
    (if entry
	(cdr entry))))
