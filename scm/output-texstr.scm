
(define-module (scm output-texstr))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (srfi srfi-13)
 (lily))


(define (dummy . foo) #f)
(map (lambda (x) (module-define! this-module x dummy))
     (append
      (ly:all-stencil-expressions)
      (ly:all-output-backend-commands)))


(define-public (text font s)
  (call-with-output-string
   (lambda (port)
     (write (list
	     "HOI"
	     (ly:font-file-name font)
	     (ly:font-magnification font)
	     s
	     ) port))))

