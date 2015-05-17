
\header {
  texidoc = "This file gives a different result each time it is run, so
it should always show up in the output-distance testing. "

}

\version "2.19.21"


#(define time (gettimeofday))
#(define random-number
  (/ (random 100 (seed->random-state (+ (car time)
  	 			        (cdr time)))) 100.0))
  
\layout {
  line-width = #(* cm (+ 5 (* 10 random-number)))
  ragged-right = ##f
}

\new PianoStaff << \new Staff \relative { c'4 d f8_\f[ g-.] r4 }
		   \new Staff { \clef "bass" R1 }
		>>

