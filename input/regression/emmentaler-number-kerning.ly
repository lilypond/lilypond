\version "2.23.10"

\header {
  texidoc = "The Emmentaler font contains kerning for many number pairs."
}

#(define pair-characters
   '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-" "+" "." ","))

#(define (number-line left-character)
   (map (lambda (x)
          (make-with-dimension-markup
           X '(0 . 5)
           (make-number-markup
            (make-concat-markup (list left-character x)))))
        pair-characters))

#(define (test-kerning title feature-list)
   (make-column-markup
    (list title
          (make-vspace-markup 1)
          (make-override-markup
           `((font-size . 0)
             (word-space . 2)
             (baseline-skip . 5)
             (font-features . ,feature-list))
           (make-column-markup
            (map (lambda (x)
                   (make-line-markup (number-line x)))
                 pair-characters))))))

$(test-kerning "time-signatures:" '(""))
\pageBreak
$(test-kerning "time signatures without kern:" '("-kern"))
\pageBreak
$(test-kerning "figured bass (ss01):" '("ss01"))
\pageBreak
$(test-kerning "figured bass (ss01) without kern:" '("ss01" "-kern"))
\pageBreak
$(test-kerning "fingering (ss02):" '("ss02"))
\pageBreak
$(test-kerning "fingering (ss02) without kern:" '("ss02" "-kern"))

\paper { print-page-number = ##f }
