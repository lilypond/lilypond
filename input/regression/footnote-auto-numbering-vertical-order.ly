\version "2.15.21"
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
        < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 2 4)
        \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
        \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
        a8\< [ b c d ] a4 b c\f |
        d a b c |\break
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 6))
        < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 8 10)
        \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
        \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
        a8\< [ b c d ] a4 b c |
        d a b c\f |\pageBreak
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 12))
        < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 14 16)
        \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
        \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
        a8\< [ b c d ] a4 b c |
        d a b c\! |\break
      }
      \new Staff \relative c' {
        d4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 1))
        < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 3 5)
        \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
        \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
        a8\< [ b c d ] a4 b c\f |
        d a b c |\break
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 7))
        < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 9 11)
        \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
        \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
        a8\< [ b c d ] a4 b c |
        d a b c\f |\pageBreak
        d,4 e
        \once \override FootnoteItem #'numbering-assertion-function =
          #(lambda (grob) (make-footnote-numbering-assertion-function 13))
        < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
        \once \override FootnoteSpanner #'numbering-assertion-function =
          #(simultaneous-footnote-numbering-assertion-function 15 17)
        \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
        \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
        a8\< [ b c d ] a4 b c |
        d a b c\! |\break
      }
    >>
  }
}
