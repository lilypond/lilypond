

(define (urlfy x)
  (string-append "<a href=" x ".html>" x "</a>"))

(define (human-listify l)
  (cond
   ((null? l) "none")
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (string-append (car l) " and " (cadr l)))
   (else (string-append (car l) ", " (human-listify (cdr l))))
   ))


(define (writing-wip x)
      (display (string-append "Writing " x " ... \n") (current-error-port))
      )

(eval-string (ly-gulp-file "generate-engraver-documentation.scm"))
(eval-string (ly-gulp-file "generate-backend-documentation.scm"))
