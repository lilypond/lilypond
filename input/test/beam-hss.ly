\header {

texidoc = "Beams in hang, straddle sit positions."

}

\version "1.5.23"

#(define ps-testing #t)
\score {
    \notes\relative c'{
        \property Voice.Beam \override #'height-quant-function
	= #(lambda (x y) '(0 4))

	\property Voice.Beam \override #'vertical-position-quant-function
 	= #(lambda (beam dy x staff-line)
 	    (let* ((thick (ly-get-grob-property beam 'thickness))
		   (hang (- 1 (/ (- thick staff-line) 2))))
	     (append (list hang) (list (+ hang 1)))))
	[d8 d]
	
	\property Voice.Beam \override #'vertical-position-quant-function
 	= #(lambda (beam dy x staff-line) '(0 1))
	[d8 d]

	\property Voice.Beam \override #'vertical-position-quant-function
 	= #(lambda (beam dy x staff-line)
 	    (let* ((thick (ly-get-grob-property beam 'thickness))
	           (sit (/ (- thick staff-line) 2)))
	     (append (list sit) (list (+ sit 1)))))
         [d8 d]
    }
    \paper{
	linewidth = 0.0
    }
}
