\version "2.25.35"

\header {
  texidoc = "Using @code{\\displayScheme} with a partial markup should not crash."
}

#(ly:set-option 'warning-as-error #t)

#(define whitespace-regex (ly:make-regex "[ \t\n]+"))

#(define (normalize-whitespace str)
   "Collapse all whitespace sequences to single spaces for comparison."
   (string-trim-both
    (ly:regex-replace whitespace-regex str " ")))

#(define (test-display-scheme markup expected)
   (let* ((output-str (call-with-output-string
                       (lambda (port)
                         (display-scheme-music markup port))))
          (output-normalized (normalize-whitespace output-str))
          (expected-normalized (normalize-whitespace expected)))
     (if (not (equal? expected-normalized output-normalized))
         (ly:warning "\
Expected output of \\displayScheme was:
~a

Actual output is:
~a

If this change is intended, please update file
  input/regression/other/display-scheme-with-partial-markup.ly."
                     expected
                     output-str))))

% Test with partial markup
\markup red = \markup \with-color #red \etc
#(test-display-scheme
  #{ \markup { \red foo } #}
  "(markup #:line (#:with-color (list 1.0 0.0 0.0) \"foo\"))")


% Test with regular markup
#(test-display-scheme
  #{ \markup { \with-color #red foo } #}
  "(markup #:line (#:with-color (list 1.0 0.0 0.0) \"foo\"))")
