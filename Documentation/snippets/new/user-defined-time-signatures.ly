\version "2.19.16"

\header {
  lsrtags = "rhythms"

  texidoc = "
New time signature styles can be defined.  The time signature in
the second measure should be upside down in both staves.
"

  doctitle = "User defined time signatures"
}

#(add-simple-time-signature-style 'topsy-turvy
   (lambda (fraction)
     (make-rotate-markup 180 (make-compound-meter-markup fraction))))

<<
  \new Staff {
    \time 3/4 f'2.
    \override Score.TimeSignature.style = #'topsy-turvy
    \time 3/4 R2. \bar "|."
  }
  \new Staff {
    R2. e''
  }
>>
