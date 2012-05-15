\version "2.15.25"
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

\markup {
  a \auto-footnote b c
  \auto-footnote d e
  \auto-footnote f g
}

\markup { h i }

\relative c' {
  \once \override FootnoteItem #'numbering-assertion-function =
    #(lambda (grob) (make-footnote-numbering-assertion-function 3))
  \footnote #'(1 . -1) #'NoteHead \markup { j }
  a b c d
}

\pageBreak

\markup { k \auto-footnote l m }

\relative c' { a1 }

\relative c' {
  d4 e
  \once \override FootnoteItem #'numbering-assertion-function =
    #(lambda (grob) (make-footnote-numbering-assertion-function 5))
  < f  a-\footnote #'(1 . -1) \markup { n } c >
  \once \override FootnoteSpanner #'numbering-assertion-function =
    #(simultaneous-footnote-numbering-assertion-function 6 7)
  \footnote #'(1 . 1) #'Beam \markup { o }
  \footnote #'(1 . 1) #'Hairpin \markup { p }
  a8\< [ b c d ] a4 b c |
  d a b c |
  d a b c |
  d a b c\f |
}}
