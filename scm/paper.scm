;;;; paper.scm -- manipulate the paper and layout block.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-public (set-paper-dimension-variables mod)
  (module-define! mod 'dimension-variables
		  '(pt mm cm in staffheight staff-space
		       betweensystemspace betweensystempadding
		       linewidth indent hsize vsize
		       staffspace linethickness ledgerlinethickness
		       blotdiameter interscoreline leftmargin rightmargin)))

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

    (module-define! m 'outputscale ss)
    (module-define! m 'fonts (make-cmr-tree (/  sz (* 20 pt))))
    (module-define! m 'staffheight sz)
    (module-define! m 'staff-space ss)
    (module-define! m 'staffspace ss)

    ;; !! synchronize with feta-params.mf
    (module-define! m 'linethickness lt)
    (module-define! m 'ledgerlinethickness (+ (* 0.5 pt) (/ ss 10)))
    (module-define! m 'blotdiameter (* 0.35 pt))
    (module-define! m 'interscoreline (* 4 mm))))

(define-public (set-global-staff-size sz)
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
	(ly:warn "Not in toplevel scope"))
    (set-current-module new-scope)
    (layout-set-staff-size (* sz (eval 'pt new-scope)))
    (set-current-module old-mod)
    (module-define! old-mod '$defaultpaper new-paper)))

(define-public paper-alist
  '(("a6" . (cons (* 105 mm) (* 148.95 mm)))
    ("a5" . (cons (* 148.95 mm) (* 210 mm)))
    ("a4" . (cons (* 210 mm) (* 297.9 mm)))
    ("a3" . (cons (* 297.9 mm) (* 420 mm)))
    ("legal" . (cons (* 8.5 in) (* 14.0 in)))
    ("letter" . (cons (* 8.5 in) (* 11.0 in)))
    ("tabloid" . (cons (* 11.0 in) (* 17.0 in)))))

;; todo: take dimension arguments.

(define (set-paper-dimensions m w h)
  "M is a module (i.e. layout->scope_ )"
  (let* ((mm (eval 'mm m)))
    (module-define! m 'hsize w)
    (module-define! m 'vsize h)
    (module-define! m 'linewidth (- w (* 20 mm)))
    (module-define! m 'indent (/ w 14))

    ;; page layout - what to do with (printer specific!) margin settings?
    (module-define! m 'topmargin (* 5 mm))
    (module-define! m 'bottommargin (* 6 mm))
    (module-define! m 'headsep (* 4 mm))
    (module-define! m 'footsep (* 4 mm))
    (module-define! m 'leftmargin #f)
    (module-define! m 'firstpagenumber 1)
    (module-define! m 'printfirstpagenumber #f)
    (module-define! m 'rightmargin (* 10 mm))))

(define (internal-set-paper-size module name landscape?)
  (define (swap x)
    (cons (cdr x) (car x)))
  
  (let* ((entry (assoc name paper-alist))
	 (is-paper? (module-defined? module 'is-paper))
	 (mm (eval 'mm module)))
    
    (cond
     ((not is-paper?)
      (ly:warning "This is not a \\layout {} object, ~S"
		   module))
     ((pair? entry)

      (set! entry (eval (cdr entry) module))
      (if landscape?
	  (set! entry (swap entry)))
      (set-paper-dimensions module (car entry) (cdr entry))

      (module-define! module 'papersizename name)

      (if landscape?
	  (module-define! module 'landscape #t)))
     (else
      (ly:warn (string-append "Unknown papersize: " name))))))

(define-public (set-default-paper-size name . rest)
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
      (ly:warn "Must use #(set-paper-size .. ) within \\paper { ... }")))

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

	     ;; spurious warnings, eg. for hsize, vsize. 
	     ;; (ly:warn "not a number, ~S = ~S " v  val)
	     )))
     
     dim-vars)
    
    new-pap))
