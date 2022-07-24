\version "2.23.14"

\header {
  texidoc = "When using the @code{-dcompile-scheme-code} option,
Scheme errors point at the precise form where the error occurs
(this is limited by the precision of the information Guile provides)."
}

% Error will point to "#(begin" line
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

#(ly:set-option 'compile-scheme-code)

% Error should point to a more specific line, although it might
% not point to the "(1+ x)" line because Guile does not always
% retain all stack info.
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
