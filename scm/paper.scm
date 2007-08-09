;;;; paper.scm -- manipulate the paper and layout block.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-public (set-paper-dimension-variables mod)
  (module-define! mod 'dimension-variables
		  '(pt mm cm in staff-height staff-space
		       page-top-space
		       between-system-space between-system-padding
		       line-width indent paper-width paper-height horizontal-shift
		       staff-space line-thickness ledgerline-thickness
		       blot-diameter left-margin right-margin)))

(define-public (layout-set-staff-size sz)
  "Function to be called inside a \\layout{} block to set the staff size."
  (let* ((m (current-module))
	 (ss (/ sz 4))
	 (pt (eval 'pt m))

	 
	 ;; linear interpolation.
	 (x1 (* 4.125 pt))
	 (x0 (* 5 pt))
	 (f1 (* 0.47 pt))
	 (f0 (* 0.50 pt))
	 (lt (/
	      (+
	       (* f1 (- ss x0))
	       (* f0 (- x1 ss)))
	      (- x1 x0)))
	 
	 (mm (eval 'mm m)))

    (module-define! m 'text-font-size (* 12 (/ sz (* 20 pt))))
    
    (module-define! m 'output-scale ss)
    (module-define! m 'fonts
		    (if tex-backend?
			(make-cmr-tree (/  sz (* 20 pt)))
			(make-century-schoolbook-tree
			 (/ sz (* 20 pt)))))
    (module-define! m 'staff-height sz)
    (module-define! m 'staff-space ss)
    (module-define! m 'staff-space ss)

    ;; !! synchronize with feta-params.mf
    (module-define! m 'line-thickness lt)
    (module-define! m 'ledgerline-thickness (+ (* 0.5 pt) (/ ss 10)))
    (module-define! m 'blot-diameter (* 0.4 pt))
    ))

(define-safe-public (set-global-staff-size sz)
  "Set the default staff size, where SZ is thought to be in PT."
  (let* ((old-mod (current-module))
	 (pap (eval '$defaultpaper old-mod))
	 (in-layout? (or (module-defined? old-mod 'is-paper)
			 (module-defined? old-mod 'is-layout)))

	 ; maybe not necessary.
	 ; but let's be paranoid. Maybe someone still refers to the
	 ; old one. 
	 (new-paper (ly:output-def-clone pap))
	 
	 (new-scope (ly:output-def-scope new-paper)))
    
    (if in-layout?
	(ly:warning (_ "Not in toplevel scope")))
    (set-current-module new-scope)
    (layout-set-staff-size (* sz (eval 'pt new-scope)))
    (set-current-module old-mod)
    (module-define! old-mod '$defaultpaper new-paper)))

(define-public paper-alist

  ;; don't use decimals.
  ;; ISO 216 has a tolerance of +- 2mm
  
  '(("a7" . (cons (* 74 mm) (* 105 mm)))
    ("a6" . (cons (* 105 mm) (* 148 mm)))
    ("a5" . (cons (* 148 mm) (* 210 mm)))
    ("a4" . (cons (* 210 mm) (* 297 mm)))
    ("a3" . (cons (* 297 mm) (* 420 mm)))
    ("legal" . (cons (* 8.5 in) (* 14.0 in)))
    ("letter" . (cons (* 8.5 in) (* 11.0 in)))
    ("11x17" . (cons (* 11.0 in) (* 17.0 in)))
    ))

;; todo: take dimension arguments.

(define (set-paper-dimensions m w h)
  "M is a module (i.e. layout->scope_ )"
  (let* ((mm (eval 'mm m)))
    (module-define! m 'paper-width w)
    (module-define! m 'paper-height h)
    (module-define! m 'line-width (- w
				     (ly:modules-lookup (list m) 'left-margin (* 10 mm))
				     (ly:modules-lookup (list m) 'right-margin (* 10 mm))))

    (module-define! m 'indent (/ w 14))

    ;; page layout - what to do with (printer specific!) margin settings?

    ))

(define (internal-set-paper-size module name landscape?)
  (define (swap x)
    (cons (cdr x) (car x)))
  
  (let* ((entry (assoc name paper-alist))
	 (is-paper? (module-defined? module 'is-paper))
	 (mm (eval 'mm module)))
    
    (cond
     ((not is-paper?)
      (ly:warning (_ "This is not a \\layout {} object, ~S" module)))
     ((pair? entry)

      (set! entry (eval (cdr entry) module))
      (if landscape?
	  (set! entry (swap entry)))
      (set-paper-dimensions module (car entry) (cdr entry))

      (module-define! module 'papersizename name)
      (module-define! module 'landscape 
		      (if landscape? #t #f)))
     (else
      (ly:warning (_ "Unknown papersize: ~a" name))))))

(define-safe-public (set-default-paper-size name . rest)
  (internal-set-paper-size
   (ly:output-def-scope (eval '$defaultpaper (current-module)))
   name
   (memq 'landscape rest)))

(define-public (set-paper-size name . rest)
  (if (module-defined? (current-module) 'is-paper)
      (internal-set-paper-size (current-module) name
			       (memq 'landscape rest))

      ;;; TODO: should raise (generic) exception with throw, and catch
      ;;; that in parse-scm.cc
      (ly:warning (_ "Must use #(set-paper-size .. ) within \\paper { ... }"))))

(define-public (scale-layout pap scale)
  (let* ((new-pap (ly:output-def-clone pap))
	 (dim-vars (ly:output-def-lookup pap 'dimension-variables))
	 (old-scope (ly:output-def-scope pap))
	 (scope (ly:output-def-scope new-pap)))

    (for-each
     (lambda (v)
       (let* ((var (module-variable old-scope v))
	      (val (if (variable? var) (variable-ref var) #f)))

	 (if (number? val)
	     (module-define! scope v
			     (/ val scale))

	     ;; spurious warnings, eg. for paper-width, paper-height. 
	     ;; (ly:warning (_ "not a number, ~S = ~S " v  val))
	     )))
     
     dim-vars)
    
    new-pap))
