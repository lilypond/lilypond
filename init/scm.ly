\scm "

(define (add-column p) (display \"adding column (in guile): \") (display p) (newline))

(define
  (control->string c)
  (string-append
    (string-append (number->string (car c)) \" \")
    (string-append (number->string (cadr c)) \" \")))

(define 
  (dashed-slur-ps thick dash l)
  (string-append 
    (apply string-append (map control->string l)) 
    (number->string thick) 
   \" [ 0 \" (number->string dash) \" ] 0 draw_dashed_slur\"))

(define 
  (dashed-slur-tex thick dash l)
  (string-append 
    \"\\embedded_ps{\"
    (dashed-slur-ps thick dash l)
   \"}\"))

(define 
  (dashed-slur o thick dash l) 
  ((eval-string (string-append \"dashed-slur-\" o)) thick dash l))

";

