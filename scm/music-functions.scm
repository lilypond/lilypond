
(define (denominator-tuplet-formatter mus)
  (number->string (ly-get-mus-property mus 'denominator)))

(define (fraction-tuplet-formatter mus)
  (string-append (number->string (ly-get-mus-property mus 'numerator))
		 ":"
		 (number->string (ly-get-mus-property mus 'denominator))
		 ))

(define (unfold-repeats music)
"
[Rune Zedeler]

Han-Wen Nienhuys wrote:

> It shouldn't be hard to write a Scheme function to replace all repeats
> with unfold repeats.
[...]
> Left to the reader as an exercise.

With thanks to Han-Wen:


"
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (body (ly-get-mus-property music 'body))
         (alts (ly-get-mus-property music 'alternatives))
         (n  (ly-music-name music)))

    (if (equal? n "Repeated_music")
        (begin
          (ly-set-mus-property
           music 'length Repeated_music::unfolded_music_length)
          (ly-set-mus-property
           music 'iterator-ctor Unfolded_repeat_iterator::constructor)))

    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map unfold-repeats es)))

    (if (music? alts)
        (ly-set-mus-property
         music 'alternatives
         (unfold-repeats alts)))

    (if (music? body)
        (ly-set-mus-property
         music 'body
         (unfold-repeats body)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (unfold-repeats e)))


    music))

