\version "1.5.18"

\header {
texidoc="
@example
    Here's a copy of my feature request :
@quotation
        Your task, if you accept it is to implement a \smarttranspose
        command> that would translate such oddities into more natural
        notations. Double accidentals should be removed, as well as E-sharp
        (-> F), bC (-> B), bF (-> E), B-sharp (-> C).
@end quotation

You mean like this. (Sorry 'bout the nuked indentation.)

Modified to use the standard transpose mechanism. The question is
how useful these enharmonic modifications are. Mats B.
@end example
"
}

#(define  (unhair-pitch p)
  (let* ((o (pitch-octave p))
         (a (pitch-alteration p))
         (n (pitch-notename p)))

    (cond
     ((and (> a 0) (or (eq? n 6) (eq? n 2)))
      (set! a (- a 1)) (set! n (+ n 1)))
     ((and (< a 0) (or (eq? n 0) (eq? n 3)))
      (set! a (+ a 1)) (set! n (- n 1))))

    (cond
     ((eq? a 2)  (set! a 0) (set! n (+ n 1)))
     ((eq? a -2) (set! a 0) (set! n (- n 1))))

    (if (< n 0) (begin (set!  o (- o 1)) (set! n (+ n 7))))
    (if (> n 6) (begin (set!  o (+ o 1)) (set! n (- n 7))))

    (make-pitch o n a)))

#(define (simplify music)
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (p (ly-get-mus-property music 'pitch)))

    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map (lambda (x) (simplify x)) es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (simplify e)))

    (if (pitch? p)
        (begin
          (set! p (unhair-pitch p))
          (ly-set-mus-property music 'pitch p)))

    music))

music = \notes \relative c' { c4 d  e f g a b  c }

\score {
  \notes \context Staff {
    \transpose ais' \music
    \apply #simplify \transpose ais' \music
  }
  \paper { linewidth = -1. }
}

