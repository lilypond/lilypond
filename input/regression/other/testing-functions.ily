\version "2.23.0"

#(define (expect-equal description actual expected)
  (if (not (equal? actual expected))
   (ly:input-warning (*location*) "case: ~a
  expected: ~a
  actual  : ~a" description expected actual)))

testStartAndLength =
#(define-void-function (music expected-start expected-length)
  (ly:music? ly:moment? ly:moment?)
  (expect-equal "start" (ly:music-start music) expected-start)
  (expect-equal "length" (ly:music-length music) expected-length))
