\version "1.3.146"

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
    (if (> n 7) (begin (set!  o (+ o 1)) (set! n (- n 7))))

    (make-pitch o n a)))

#(define (smart-transpose music pitch)
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (p (ly-get-mus-property music 'pitch))
         (body (ly-get-mus-property music 'body))
         (alts (ly-get-mus-property music 'alternatives)))

    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map (lambda (x) (smart-transpose x pitch)) es)))

    (if (music? alts)
        (ly-set-mus-property
         music 'alternatives
         (smart-transpose alts pitch)))

    (if (music? body)
        (ly-set-mus-property
         music 'body
         (smart-transpose body pitch)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (smart-transpose e pitch)))

    (if (pitch? p)
        (begin
          (set! p (unhair-pitch (Pitch::transpose p pitch)))
          (ly-set-mus-property music 'pitch p)))

    music))


music = \notes \relative c' { c4 d  e f g a b  c }

\score {
  \notes \context Staff {
    \transpose ais' \music
    \apply #(lambda (x) (smart-transpose x (make-pitch 0 5 1)))
      \music
  }
  \paper { linewidth = -1. }
}

