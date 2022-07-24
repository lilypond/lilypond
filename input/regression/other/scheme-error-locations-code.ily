\version "2.23.14"

#(begin
  (define (f x)
    (1+ (g x)))
  (define (g x)
    (1+ (h x)))
  (define (h x)
    (1+ (i x)))
  (define (i x)
    (1+ x)))

