\version "2.23.14"

\header {
  texidoc = "Test skyline merging.  The red and blue lines are
two skylines with direction @code{UP}, represented with the X axis
as horizon axis.  The black line is the merged skyline.  At every
point on the X axis, the black line should be at the maximum between
the height of the red line and the height of the blue line at that
point."
}

pointsI =
#'((-inf.0 -inf.0 1 -inf.0)
   (1 0 2 3)
   (2 -3 3 -3)
   (4 -5 6 5)
   (6 5 7 -5)
   (7 -5 12 -5)
   (13 0 16 0)
   (18 0 20 0))

pointsII =
#'((-inf.0 -1 0 -1)
   (0 -1 10 0)
   (11 -7 13 0)
   (14 1 17 -1)
   (17 -1 19 -1))

skyI = #(ly:make-skyline pointsI X UP)
skyII = #(ly:make-skyline pointsII X UP)
merged = #(ly:skyline-merge skyI skyII)

#(define (successive-pairs lst)
   (let loop ((lst lst)
              (acc '()))
     (match lst
      ((fst . (and rest (snd . _)))
       (loop rest
             (cons (cons fst snd)
                   acc)))
      ((or (single) ())
       (reverse! acc)))))

#(define (make-finite x)
   (case x
     ((-inf.0) -20)
     ((+inf.0) +20)
     (else x)))

#(define (show-skyline sky)
   (let ((points (ly:skyline->points sky X)))
     (apply ly:stencil-add
           (map (match-lambda
                 (((x1 . y1) . (x2 . y2))
                  (make-line-stencil
                   0.1
                   (make-finite x1)
                   (make-finite y1)
                   (make-finite x2)
                   (make-finite y2))))
                (successive-pairs points)))))

\markup \overlay {
  \with-color "red" \stencil #(show-skyline skyI)
  \with-color "blue" \stencil #(show-skyline skyII)
  \raise #0.05 \stencil #(show-skyline merged)
}
