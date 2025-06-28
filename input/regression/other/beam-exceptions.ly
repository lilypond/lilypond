\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beam-exceptions} Scheme
function for single-fraction time signatures.  Problems are reported in the
standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "signature not found in settings"
  (beam-exceptions '(6 . 7) '())
  '())
#(expect-equal "signature not found in settings; senza misura"
  (beam-exceptions #f '())
  '())
#(expect-equal "signature not found in settings; zero denominator"
  (beam-exceptions '(1 . 0) '())
  '())

#(expect-equal "exceptions not found in settings"
  (beam-exceptions '(6 . 7) '(((6 . 7) . ())))
  '())
#(expect-equal "exceptions not found in settings; senza misura"
  (beam-exceptions #f '((#f . ())))
  '())
#(expect-equal "exceptions not found in settings; zero denominator"
  (beam-exceptions '(1 . 0) '(((1 . 0) . ())))
  '())

#(expect-equal "exceptions found in settings"
  (beam-exceptions
   '(6 . 8)
   '(((6 . 8) . ((beamExceptions . dummy1)))))
  'dummy1)
#(expect-equal "exceptions found in settings; senza misura"
  (beam-exceptions
   #f
   '((#f . ((beamExceptions . dummy2)))))
  'dummy2)
#(expect-equal "exceptions found in settings; zero denominator"
  (beam-exceptions
   '(1 . 0)
   '(((1 . 0) . ((beamExceptions . dummy3)))))
  'dummy3)
