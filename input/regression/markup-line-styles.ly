\version "2.19.22"

\header {
  texidoc = "The markup-commands @code{\\draw-dashed-line},
  @code{\\draw-dotted-line} and @code{\\draw-squiggle-line} should print the
  same visual length as @code{\\draw-line}.
  Also testing possible overrides for @code{\\draw-squiggle-line}"
}

%% draw-dotted-line and draw-dashed-line
test =
#(define-scheme-function (x-nmbr y-nmbr)(number? number?)
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

%% draw-squiggle-line
mrkp =
\markup
  \override #'(word-space . 2)
  \column {
    \line { \draw-squiggle-line #0.5 #'(6 . 0) ##t \tiny \vcenter "default" }
    \line {
      \override #'(orientation . -1) \draw-squiggle-line #0.5 #'(6 . 0) ##t
      \tiny \vcenter "different orientation"
    }
    \line {
      \draw-squiggle-line #0.5 #'(6 . 0) ##f
      \tiny \vcenter "\"eq-end?\" set #f"
    }
    \line {
      \override #'(height . 1) \draw-squiggle-line #0.5 #'(6 . 0) ##t
      \tiny \vcenter "different height"
    }
    \line {
      \override #'(thickness . 5) \draw-squiggle-line #0.5 #'(6 . 0) ##t
      \tiny \vcenter "different thickness"
    }
    \line {
      \override #'(angularity . 2) \draw-squiggle-line #0.5 #'(6 . 0) ##t
      \tiny \vcenter "different angularity"
    }
  }

test-draw-squiggle-line =
#(define-scheme-function (steps) (integer?)
;; Puts out a markup combining draw-line-markup and draw-squiggle-line-markup
;; in a  helix-like mannor
  (define (val-pts-list steps)
    ;; Puts out a list, with each element being a pair of a numerical value
    ;; and a number-pair
    ;; The numerical value is used for first-bow-length and its height
    ;; The number-pair is the destination-point of the line.
    ;; Those points are on a simple helix around '(0 . 0)
    (map
      (lambda (n r)
        (let* ((y (* (sin n) r))
               (x (* (cos n) r)))
          (if (< (abs x) 0.00001)
              (set! x 0))
          (if (< (abs y) 0.00001)
              (set! y 0))
          (cons (max 0.1 (- 0.5 (/ 1 r))) (cons x y ))))
      (iota steps 0 (/ TWO-PI steps))
      (iota steps 3 0.5)))

  (let ((args
          (map
            (lambda (arg)
              #{
                \markup
                  \combine
                    \draw-line $(cdr arg)
                    \override #`(height . , (car arg))
                    \draw-squiggle-line
                      #(car arg)
                      $(cdr arg) ##f
              #})
            (val-pts-list steps))))
    #{ \markup { \hspace #10 \overlay $args \hspace #5 \vcenter \mrkp } #}))

\test #15 #0

\test-draw-squiggle-line #12 #10
