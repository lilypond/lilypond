\version "2.23.4"
\header {
  texidoc = "This is an example of automatic footnote numbering
where the number is not reset on each page.  It uses the default
numbering function, which assigns numbers starting at 1 to successive
footnotes.
"
}

#(define (make-footnote-numbering-assertion-function n)
  (lambda (x)
    (if (not (= n x))
      (ly:error (G_ "Expecting number ~a, got ~a") n x))))

#(define (simultaneous-footnote-numbering-assertion-function x y)
  (lambda (grob)
    (let ((n (if (grob::has-interface (ly:grob-parent grob Y)
                                      'beam-interface)
                 x
                 y)))
      (lambda (x)
        (if (not (= n x))
          (ly:error (G_ "Expecting number ~a, got ~a") n x))))))

\paper {
  reset-footnotes-on-new-page = ##f
}

#(set-default-paper-size "a6")
\book {

\markup {
  a \auto-footnote b c
  \auto-footnote d e
  \auto-footnote f g
}

\markup { h i }

\relative c' {
  \once \override Score.Footnote.numbering-assertion-function =
    #(lambda (grob) (make-footnote-numbering-assertion-function 3))
  \footnote #'(1 . -1) \markup { j } NoteHead
  a b c d
}

\pageBreak

\markup { k \auto-footnote l m }

\relative { a1 }

\relative {
  d'4 e
  \once \override Score.Footnote.numbering-assertion-function =
    #(lambda (grob) (make-footnote-numbering-assertion-function 5))
  < f \footnote #'(1 . -1) \markup { n } a c >
  \once \override Score.Footnote.numbering-assertion-function =
    #(simultaneous-footnote-numbering-assertion-function 6 7)
  a8-\footnote #'(1 . 1) \markup { p } \<
  -\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c |
  d a b c |
  d a b c |
  d a b c\f |
}}
