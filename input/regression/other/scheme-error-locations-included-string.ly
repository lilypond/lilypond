\version "2.23.14"

\header {
  texidoc = "@code{-dcompile-scheme-code} also works with
expressions included by @code{ly:parser-include-string}
and @code{ly:parser-parse-string}.  This test should print
two Scheme errors.  (However, providing error messages that
point to the precise form that fails is not currently
implemented for included strings of code.)"
}

#(ly:set-option 'compile-scheme-code)

#(ly:parser-include-string
  "#(begin
      (define (f x)
        (1+ (g x)))
      (define (g x)
        (1+ (h x)))
      (define (h x)
        (1+ (i x)))
      (define (i x)
        (1+ x))
      (f 'oops))")

#(ly:parser-parse-string
  (ly:parser-clone)
  "#(begin
      (define (f x)
        (1+ (g x)))
      (define (g x)
        (1+ (h x)))
      (define (h x)
        (1+ (i x)))
      (define (i x)
        (1+ x))
      (f 'oops))")
