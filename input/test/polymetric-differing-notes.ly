#(ly:set-option 'old-relative)
\version "1.9.8"

\header{ texidoc="

@cindex polymetric music

@cindex scaling durations

You can have multiple time signatures occuring at the same time, with
different durations aligned.  This is done by 1. compressing one of
the lines, analogous to \times, but without the bracket, and
2. manually setting timeSignatureFraction to the desired fraction.

This example puts 3/4, 9/8 and 10/8 in parallel. The last staff shows
what happens on the inside: a 3/4 time signature is combined with a
3/5 tuplet yielding the equivalent of a 10/8.

"

}


#(define (scale-one-music m fraction)
  "Maybe we should just export Music::compress to Scheme?"
  (let*
   ((dur (ly:get-mus-property m 'duration)))
   
   (if (ly:duration? dur)
    (let*
     ((l (ly:duration-log dur))
      (d (ly:duration-dot-count dur))
      (factor (ly:duration-factor dur)))

      (ly:set-mus-property! m 'duration
                            (ly:make-duration l d
			     (* (car fraction) (car factor))
			     (* (cdr fraction) (cdr factor))))))
   
   m))

#(define (scale-music-function fraction)
  (lambda (x) 
   (music-map (lambda (y) (scale-one-music y fraction)) x)))



\score {
    \notes \relative c'  <<
    	\new Staff {
	    \time 3/4
	    c4 c c | c c c |
	}
    	\new Staff {
	    \time 3/4
	    \property Staff.timeSignatureFraction= #'(9 . 8)
	    \apply #display-music \apply #(scale-music-function '(2 . 3))
	      \repeat unfold 6 { c8[ c c] }
	}
	
    	\new Staff {
	    \time 3/4
	    \property Staff.timeSignatureFraction= #'(10 . 8)
	    \apply #display-music \apply #(scale-music-function '(3 . 5))
	      { \repeat unfold 2 { c8[ c c] }
		\repeat unfold 2 { c8[  c] }
		|  c4. c4. \times 2/3 { c8 c c } c4  }
	}
	>>
	\paper { raggedright = ##t }
}
