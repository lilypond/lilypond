; Definition of backend properties (aka. element properties).

;; See documentation of Item::visibility_lambda_
(define (begin-of-line-visible d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define (spanbar-begin-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))
(define (all-visible d) '(#f . #f))
(define (all-invisible d) '(#t . #t))
(define (begin-of-line-invisible d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define (end-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))


(define mark-visibility end-of-line-invisible)

; ugh: should calculate from beam-thickness.
; result in staff-space

;beam_thickness = 0.52 * (\staffspace - \stafflinethickness);
;interbeam = (2.0 * \staffspace + \stafflinethickness - \beam_thickness) / 2.0;
;interbeam4 = (3.0 * \staffspace - \beam_thickness) / 3.0;


(define (default-beam-space-function multiplicity)
  (if (<= multiplicity 3) 0.816 0.844)
  )

;
; width in staff space.
;
(define (default-beam-flag-width-function type)
  (cond
   ((eq? type 1) 1.98)
   ((eq? type 1) 1.65)
   (else 1.32)
   ))


(define basic-beam-properties
  `(
    (interfaces . (beam-interface))
    (molecule-callback . ,Beam::brew_molecule)
    (beam-thickness . 0.42) ; staff-space, should use stafflinethick?
    (before-line-breaking-callback . ,Beam::before_line_breaking)
    (after-line-breaking-callback . ,Beam::after_line_breaking)
    (default-neutral-direction . 1)
    
    (beam-flag-width-function . ,default-beam-flag-width-function)
    (beam-space-function . ,default-beam-space-function)
    (damping . 1)
    (name . "beam")		
    )
  )
