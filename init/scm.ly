\scm "
(define (add-column p) (display \"adding column (in guile): \") (display p) (newline))
(define (dashed-slur o l) (newline) (display \"output type: \") (display o) (newline) (display \"controls: \") (map display l) (newline))
";

% deze is blond
% ((lambda (o) (dashed-slur o '((0.0 0.0) (17.2124966172942 -16.8303258151405) (161.324688856776 -15.2124966172942) (178.155014671916 2.0)))) 'ps)

