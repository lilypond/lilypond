\header { texidoc= "

Marks can be printed as numbers.  
By setting @code{markFormatter} we may choose a different style of mark printing. Also, marks can be specified manually, with a markup argument." 
	  
      }

\version "2.3.16"
\score {
   \relative c''{
    \set Score.markFormatter = #format-mark-numbers 
  c1 | \mark \markup { \musicglyph #"scripts-coda" }
  c1 | \mark \default
  c1 | \mark \default
    \set Score.markFormatter = #(lambda (mark  context)
	(make-bold-markup (make-box-markup (number->string mark))))
	
    c1 | \mark \default
    c1 | \mark \default
  }
}

