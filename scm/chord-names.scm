;; pitch: (notename . accidental)
;; list:  (list-of-pitches . (modifier-string . addition-subtraction-string))

;; if a complete chord is found, use name
;; if a chord's base triad is found (c e g), use name

(define pitch-names-alist '())
(set! pitch-names-alist
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
      pitch-names-alist))

(define (user-pitch-name pitch)
  (let ((entry (assoc pitch pitch-names-alist)))
       (if entry
	   (cdr entry))))

(define chord-names-alist '())
(set! chord-names-alist
      (append 
      '(
	; C iso C.no3.no5
	(((0 . 0)) . ("" . ""))
	; C iso C.no5
	(((0 . 0) (2 . 0)) . ("" . ""))
	; Cm iso Cm.no5
	(((0 . 0) (2 . -1)) . ("m" . ""))
	; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ("dim" . ""))
	; Co iso Cm5-7-
	; urg
        ; (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" . ("feta-1" . ".")))
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" . ("script" . "o")))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1)) . ("dim" . ("script" . "9")))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1)) . ("dim" . ("script" . "11")))
	)
      chord-names-alist))

(define (user-chord-name chord)
  ;(display chord)
  ;(newline)
  (let ((entry (assoc chord chord-names-alist)))
       (if entry
	   (cdr entry))))

