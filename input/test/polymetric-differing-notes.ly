\version "2.3.8"

\header{ texidoc="

@cindex polymetric music

@cindex scaling durations

It is possible to have multiple time signatures, each one in an own staffs, 
at the same time, and have even a proper vertical alignment of the different 
durations.  This is done, firstly, by setting a common time signature for
each staff but replacing it manually using @code{timeSignatureFraction} to 
the desired fraction, and secondly, by scaling the printed durations to
the actual, polymetric durations.

In this example, music with the time signatures of 3/4, 9/8 and 10/8 are
used in parallel. In the second staff, shown durations are multiplied by 
2/3, so that 2/3 * 9/8 = 3/4, and in the third staff, shown durations are 
multiplied by 3/5, so that 3/5 * 10/8 = 3/4.

"

}

#(define ((scale-music-function fraction) x)
  (ly:music-compress x (ly:make-moment (car fraction) (cdr fraction))))




\score {
     \relative c'  <<
    	\new Staff {
	    \time 3/4
	    c4 c c | c c c |
	}
    	\new Staff {
	    \time 3/4
	    \set Staff.timeSignatureFraction = #'(9 . 8)
	    \applymusic #(scale-music-function '(2 . 3))
	      \repeat unfold 6 { c8[ c c] }
	}
	
    	\new Staff {
	    \time 3/4
	    \set Staff.timeSignatureFraction = #'(10 . 8)
	    \applymusic #(scale-music-function '(3 . 5))
	      { \repeat unfold 2 { c8[ c c] }
		\repeat unfold 2 { c8[  c] }
		|  c4. c4. \times 2/3 { c8 c c } c4  }
	}
	>>
	\paper { raggedright = ##t }
}
