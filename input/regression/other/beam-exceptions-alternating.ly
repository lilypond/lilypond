\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beam-exceptions} Scheme
function for strictly alternating time signatures.  Problems are reported in the
standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "no signatures found in settings"
  (beam-exceptions
   '((1 . 11) (4 . 17) ((2 3) . 7))
   '())
  '())

%% We don't expect a strictly alternating time signature to have its own entry
%% in the settings, but there is value in testing it.
#(expect-equal "full signature found in settings; no exceptions found"
  (beam-exceptions
   '((1 . 11) (4 . 17))
   '((((1 . 11) (4 . 17)) . ())))
  '())
#(expect-equal "full signature found in settings; exceptions found"
  (beam-exceptions
   '((1 . 11) (4 . 17))
   '((((1 . 11) (4 . 17)) . ((beamExceptions . dummy)))))
  'dummy)

#(expect-equal "first component exceptions found in settings"
  (beam-exceptions
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beamExceptions . ((end . ((1/20 . (4 3 3))
                                            (1/16 . (3 3 2))))))))))
  '((end . ((1/16 . (3 3 2  4 4 4))
            (1/20 . (4 3 3  5 5 5))))))

#(expect-equal "second component exceptions found in settings"
  (beam-exceptions
   '((2 . 2) (3 . 4))
   '(((2 . 2) . (; default beatBase is 1/2
                (beatStructure . (2))))
     ((3 . 4) . ((beamExceptions . ((end . ((1/8 . (1 2 3))))))))))
  '((end . ((1/8 . (8  1 2 3))))))

#(expect-equal "exceptions for mixed beam durations"
  (beam-exceptions
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beamExceptions . ((end . ((1/8  . (2 2))
                                            (1/16 . (3 5))))))))
     ((3 . 4) . ((beamExceptions . ((end . ((1/12 . (6 3))))))))))
  '((end . ((1/8 . (2 2  2 2 2))
            (1/12 . (3 3  6 3))
            (1/16 . (3 5  8 4))))))

#(expect-equal "component exceptions not found; zero numerator"
  (beam-exceptions
   '((1 . 2) (0 . 3))
   '(((1 . 2) . ((beamExceptions . ((end . ((1/16 . (3 2 3))))))))))
  '((end . ((1/16 . (3 2 3))))))

#(expect-equal "component exceptions not found; zero denominator"
  (beam-exceptions
   '((1 . 2) (3 . 0))
   '(((1 . 2) . ((beamExceptions . ((end . ((1/16 . (3 2 3))))))))))
  '((end . ((1/16 . (3 2 3  +inf.0))))))
