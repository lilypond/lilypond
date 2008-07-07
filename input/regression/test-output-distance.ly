
\header {
  texidoc = "This file gives a different result each time it is run, so
it should always show up in the output-distance testing. "

}

\version "2.11.51"


#(define time (gettimeofday))
#(define random-number
  (/ (random 100 (seed->random-state (+ (car time)
  	 			        (cdr time)))) 100.0))
  
\layout {
  line-width = #(* cm (+ 5 (* 10 random-number)))
  ragged-right = ##f
}

\relative { c4 d f8_\f[ g-.] }

