#(define (write-cff name filename)
  (let*
      ((body (ly:gulp-file filename))
       (outname (format "~a.ps" filename))
       (out-port (open-output-file outname))
       )
    

    (display (ps-embed-cff body name 0)
	     out-port)
  ))

#(write-cff "emmentaler-26" "emmentaler26.cff")
#(write-cff "emmentaler-23" "emmentaler23.cff")
#(write-cff "emmentaler-20" "emmentaler20.cff")
#(write-cff "emmentaler-18" "emmentaler18.cff")
#(write-cff "emmentaler-16" "emmentaler16.cff")
#(write-cff "emmentaler-14" "emmentaler14.cff")
#(write-cff "emmentaler-13" "emmentaler13.cff")
#(write-cff "emmentaler-11" "emmentaler11.cff")
#(write-cff "aybabtu" "aybabtu.cff")
