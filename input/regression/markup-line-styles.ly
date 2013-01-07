\version "2.17.10"

\header {
  texidoc = "The markup-commands @code{\\draw-dashed-line} and
  @code{\\draw-dotted-line} should print the same visual length as
  @code{\\draw-line}."
}

test =
#(define-scheme-function (parser location x-nmbr y-nmbr)(number? number?)
 (let* ((lst (map
               (lambda (x)
                 (let* ((x-lngth (if (positive? x-nmbr)
                                     (* x 0.75)
                                     (* x -0.75)))
                        (dest (cons x-lngth y-nmbr))
                        (x-strg (number->string x-lngth))
                        (y-strg (number->string y-nmbr))
                        (txt-1 (markup
                               #:concat (
                                   " \\draw-dotted-line #'("
                                   x-strg
                                   " . "
                                   y-strg
                                   ")")))
                        (txt-2 (markup
                               #:concat (
                                   " \\draw-dashed-line #'("
                                   x-strg
                                   " . "
                                   y-strg
                                   ")")))
                        (txt-3 (markup
                               #:concat (
                                   " \\draw-line #'("
                                   x-strg
                                   " . "
                                   y-strg
                                   ")"))))
                    (markup
                       #:override '(baseline-skip . 0)
                       #:left-column
                         (
                         ;; dotted-line
                         #:line
                           ((#:draw-dotted-line dest)
                             #:vcenter (#:fontsize -4 txt-1))
                         ;; dashed-line
                         #:line
                           ((#:draw-dashed-line dest)
                             #:vcenter (#:fontsize -4 txt-2))
                         ;; default solid-line:
                         #:line
                           ((#:draw-line dest)
                             #:vcenter (#:fontsize -4 txt-3))
                         #:vspace 0.5))))
                  (iota (abs x-nmbr)))))
        lst))

\test #15 #0
