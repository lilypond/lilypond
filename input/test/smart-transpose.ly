
\version "2.1.28"

\header {
texidoc="@cindex Smart Transpose

There is a way to enforce enharmonic modifications for notes in order
to have the minimum number of accidentals. In that case, ``Double 
accidentals should be removed, as well as E-sharp (-> F), bC (-> B),
bF (-> E), B-sharp (-> C).'', as proposed by a request for a new feature.
In this example, the double accidentals are removed after transposing
C scale to Ais.

"
}
%
% Modified to use the standard transpose mechanism. The question is
% how useful these enharmonic modifications are. Mats B.
% 
% Why not to have a function that minimizes the number of accidentals? -HJJ
%

#(define  (unhair-pitch p)
  (let* ((o (ly:pitch-octave p))
         (a (ly:pitch-alteration p))
         (n (ly:pitch-notename p)))

    (cond
     ((and (> a 2) (or (eq? n 6) (eq? n 2)))
      (set! a (- a 2))
      (set! n (+ n 1)))
     ((and (< a -2) (or (eq? n 0) (eq? n 3)))
      (set! a (+ a 2))
      (set! n (- n 1))))

    (cond
     ((eq? a 4) (set! a 0) (set! n (+ n 1)))
     ((eq? a -4) (set! a 0) (set! n (- n 1))))

    (if (< n 0) (begin (set!  o (- o 1)) (set! n (+ n 7))))
    (if (> n 6) (begin (set!  o (+ o 1)) (set! n (- n 7))))

    (ly:make-pitch o n a)))

#(define (simplify music)
  (let* ((es (ly:music-property music 'elements))
         (e (ly:music-property music 'element))
         (p (ly:music-property music 'pitch)))

    (if (pair? es)
        (ly:music-set-property!
         music 'elements
         (map (lambda (x) (simplify x)) es)))

    (if (ly:music? e)
        (ly:music-set-property!
         music 'element
         (simplify e)))

    (if (ly:pitch? p)
        (begin
          (set! p (unhair-pitch p))
          (ly:music-set-property! music 'pitch p)))

    music))

music = \notes \relative c' { c4 d  e f g a b  c }

\score {
  \notes \context Staff {
    \transpose c ais \music
    \apply #simplify \transpose c ais \music
  }
  \paper { raggedright = ##t}
}


