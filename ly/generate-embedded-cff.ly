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

#(write-cff "Emmentaler-26" "emmentaler-26.commonff")
#(write-cff "Emmentaler-23" "emmentaler-23.commonff")
#(write-cff "Emmentaler-20" "emmentaler-20.commonff")
#(write-cff "Emmentaler-18" "emmentaler-18.commonff")
#(write-cff "Emmentaler-16" "emmentaler-16.commonff")
#(write-cff "Emmentaler-14" "emmentaler-14.commonff")
#(write-cff "Emmentaler-13" "emmentaler-13.commonff")
#(write-cff "Emmentaler-11" "emmentaler-11.commonff")
#(write-cff "aybabtu" "aybabtu.commonff")
