; paper.scm - manipulate the paper block.
;
; (C) 2004 Han-Wen Nienhuys

; the  functions are fairly basic here.



(define-public (paper-set-staff-size sz)
  "Function to be called inside a \\paper{} block to set the staff size."
  (let*
   ((m (current-module))
    (ss (/ sz 4))
    (pt (eval 'pt m)) 
    (mm (eval 'mm m))  )
   
    (module-define! m 'fonts (scale-font-list (/  sz (* 20 pt))))
    (module-define! m 'staffheight sz)
    (module-define! m 'staff-space ss)
    (module-define! m 'linethickness (* 0.5 pt))
    (module-define! m 'outputscale ss)
    (module-define! m 'ledgerlinethickness (+ (* 0.5 pt) (/ ss 10)))
    (module-define! m 'blotdiameter (* 0.35 pt))
    (module-define! m 'interscoreline (* 4 mm))
  ))

(define-public (set-staff-size sz)
  "Set the default staff size, where SZ is thought to be in PT."
  (let*
      ((old-mod (current-module))
       (pap (eval  '$defaultpaper old-mod))
       (new-paper (ly:output-def-clone pap))
       (new-scope  (ly:output-def-scope new-paper))
       )
    (set-current-module new-scope)
    (paper-set-staff-size (* sz (eval 'pt new-scope)))
    (set-current-module old-mod)
    (module-define! old-mod '$defaultpaper new-paper)
  ))


(define paper-alist
  '(("a4" . (cons (* 210 mm) (* 297.9 mm)))
    ("a3" . (cons (* 297.9 mm)  (* 420 mm)))
    ("legal" . (cons (* 8.5 in)  (* 14.0 in)))
    ("letter" . (cons (* 8.5 in) (* 11.0 in)))
    ("tabloid" . (cons (* 11.0 in) (* 17.0 in))))   )

;; todo: take dimension arguments.

(define-public (set-paper-size name)
  (let*
      ((entry (assoc name paper-alist))
       (m (current-module))
       (mm (eval 'mm m))
       )

    (if (pair? entry)
	(begin
	  (set! entry (eval  (cdr entry) m))
	  (module-define! m 'papersize name)
	  (module-define! m 'papersizename name)
	  (module-define! m 'hsize (car entry))
	  (module-define! m 'vsize (cdr entry))
	  (module-define! m 'linewidth (- (car entry) (* 20 mm)))
	  (module-define! m 'raggedright #f)
	  (module-define! m 'packed #f)
	  (module-define! m 'indent (/ (car entry) 4)) )
	(ly:warning (string-append "Unknown papersize: " name))
	)))
