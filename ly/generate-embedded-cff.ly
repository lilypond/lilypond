\version "2.4.0"
#(define (write-cff name filename)
  (let*
      ((body (ly:gulp-file filename))
       (outname (format "~a.ps" filename))
       (out-port (open-output-file outname))
       )
    

    (display (ps-embed-cff body name 0)
	     out-port)
  ))

#(write-cff "emmentaler-26" "emmentaler-26.cff")
#(write-cff "emmentaler-23" "emmentaler-23.cff")
#(write-cff "emmentaler-20" "emmentaler-20.cff")
#(write-cff "emmentaler-18" "emmentaler-18.cff")
#(write-cff "emmentaler-16" "emmentaler-16.cff")
#(write-cff "emmentaler-14" "emmentaler-14.cff")
#(write-cff "emmentaler-13" "emmentaler-13.cff")
#(write-cff "emmentaler-11" "emmentaler-11.cff")
#(write-cff "aybabtu" "aybabtu.cff")
