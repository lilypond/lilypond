
\pitchnames
#(append (map (lambda (x) (cons (car x) (caddr x))) drum-pitch-names)
	 (map (lambda (x) (cons (cadr x) (caddr x))) drum-pitch-names)
 )


\version "1.5.49"
