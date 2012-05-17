\version "2.15.39"
\header {
  texidoc = "This regtest makes sure that footnote numbers are laid out
in the correct vertical order.
"
}

#(define (make-footnote-numbering-assertion-function n)
  (lambda (x)
    (if (not (= n x))
      (ly:error (_ "Expecting number ~a, got ~a") n x))))

#(define (simultaneous-footnote-numbering-assertion-function x y)
  (lambda (grob)
    (let ((n (if (grob::has-interface (ly:grob-parent grob Y)
                                      'beam-interface)
                 x
                 y)))
      (lambda (x)
        (if (not (= n x))
          (ly:error (_ "Expecting number ~a, got ~a") n x))))))

\paper {
  reset-footnotes-on-new-page = ##f
}

#(set-default-paper-size "a6")
\book {
  \score {
    <<
      \new Staff \relative c' {
        d4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 0))
        < f \footnote #'(1 . -1) \markup { n } a c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 2 4)
        a8-\footnote #'(1 . 1) \markup { p } \<
	-\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c\f |
        d a b c |\break
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 6))
        < f \footnote #'(1 . -1) \markup { n } a c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 8 10)
        a8-\footnote #'(1 . 1) \markup { p } \<
	-\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c |
        d a b c\f |\pageBreak
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 12))
        < f  \footnote #'(1 . -1) \markup { n } a c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 14 16)
        a8-\footnote #'(1 . 1) \markup { p } \<
	-\footnote #'(1 . 1) #'Beam \markup { o } [ b c d ] a4 b c |
        d a b c\! |\break
      }
      \new Staff \relative c' {
        d4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 1))
        < f \footnote #'(1 . -1) \markup { n } a c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 3 5)
        a8-\footnote #'(1 . 1) #'Hairpin \markup { p } \<
	-\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c\f |
        d a b c |\break
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 7))
        < f \footnote #'(1 . -1) \markup { n } a c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 9 11)
        a8-\footnote #'(1 . 1) \markup { p } \<
	-\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c |
        d a b c\f |\pageBreak
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 13))
        < f \footnote #'(1 . -1) \markup { n } a c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 15 17)
        a8-\footnote #'(1 . 1) #'Hairpin \markup { p } \<
	-\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c |
        d a b c\! |\break
      }
    >>
  }
}
