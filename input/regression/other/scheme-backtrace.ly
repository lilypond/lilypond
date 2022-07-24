\version "2.23.14"

\header {
  texidoc = "When @code{#(debug-enable 'backtrace)} is used,
a Scheme errors prints a backtrace, even if it is caught by
LilyPond and compilation continues."
}

#(debug-enable 'backtrace)

#(begin
  (define (f x)
    (1+ (g x)))
  (define (g x)
    (1+ (h x)))
  (define (h x)
    (1+ (i x)))
  (define (i x)
    (1+ x))
  (f 'oops))

% Also works in -dcompile-scheme-code mode

#(ly:set-option 'compile-scheme-code)

#(begin
  (define (f x)
    (1+ (g x)))
  (define (g x)
    (1+ (h x)))
  (define (h x)
    (1+ (i x)))
  (define (i x)
    (1+ x))
  (f 'oops))
