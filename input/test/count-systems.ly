\header{
    
    texidoc="Display the number of systems, or the system number of a
    Grob.  This can be most useful to assertain that a piece uses a
    specified number of lines."
    
}

#(define (display-systemno smob)
  (let* ((this-line (get-line smob))
	 (systems (get-broken-into
		   (get-original this-line))))
    (display smob)
    (display (list-index systems this-line))
    (newline)))
  

#(define (display-system-count smob)
  (display (length
	    (get-broken-into
	     (get-original
	      (get-line smob))))))

  
  
\score{
    \notes\relative c''{
	\property Thread.NoteHead \override #'after-line-breaking-callback
	% = #display-system-count
	= #display-systemno
	c1
	d
    }
    \paper{ indent = 0.0\mm
	    linewidth = 10.0\mm
	}
}