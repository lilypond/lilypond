\header { texidoc= "

Marks can be printed as numbers.  
By setting @code{markFormatter} we may choose a different style of mark printing. Also, marks can be specified manually, with a markup argument" 
	  
      }

\version "2.1.7"


global =  \notes {
    \property Score.markFormatter = #format-mark-numbers 
  s1 | \mark \markup { \musicglyph #"scripts-coda" }
  s1 | \mark \default
  s1 | \mark \default
    \property Score.markFormatter
    = #(lambda (mark  context)
	(make-bold-markup (make-box-markup (number->string mark))))
	
    s1 | \mark \default
    s1 | \mark \default
  }


one =  \notes \relative c {
  c''1 c c c c c c 
}


\score{
\context Staff	<< \global \one >>
}

