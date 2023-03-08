\version "2.25.1"

\header {
  texidoc = "Test the behavior of regular expression
functions."
}

#(assert (equal? "\\*\\+\\$" (ly:regex-quote "*+$")))

#(assert (ly:regex-exec (ly:make-regex (ly:regex-quote "*+$"))
                        "*+$"))

#(let ((regex (ly:make-regex "x(a+)(b*)\\b")))
   (assert (eq? #f (ly:regex-exec regex "aabbcc")))
   (let ((m (ly:regex-exec regex "++xaabb**")))
     (assert (ly:regex-match? m))
     (assert (equal? "xaabb" (ly:regex-match-substring m 0)))
     (assert (equal? "aa" (ly:regex-match-substring m 1)))
     (assert (equal? "bb" (ly:regex-match-substring m 2)))
     (assert (equal? '(2 . 7) (ly:regex-match-positions m 0)))
     (assert (equal? '(3 . 5) (ly:regex-match-positions m 1)))
     (assert (equal? '(5 . 7) (ly:regex-match-positions m 2)))
     (assert (equal? "++" (ly:regex-match-prefix m)))
     (assert (equal? "**" (ly:regex-match-suffix m))))
   (assert (equal? "++xaabbxaabb++" (ly:regex-replace regex "++xaabb++" 0 0)))
   (assert (equal? "++bbaa*++" (ly:regex-replace regex "++xaabb++" 2 1 "*")))
   (assert (equal? "++bbaa++bbbaaa++"
                   (ly:regex-replace
                    regex
                    "++xaabb++xaaabbb++"
                    (lambda (m)
                      (string-append (ly:regex-match-substring m 2)
                                     (ly:regex-match-substring m 1)))))))

#(let* ((regex (ly:make-regex "(?:a(a*)|b(b*))c"))
        (m (ly:regex-exec regex "ac")))
   (assert (equal? "" (ly:regex-match-substring m 1)))
   (assert (not (ly:regex-match-substring m 2)))
   (assert (equal? '(1 . 1) (ly:regex-match-positions m 1)))
   (assert (not (ly:regex-match-positions m 2))))

#(let* ((regex (ly:make-regex "(?:a(a*)|b(b*))(c)"))
        (m (ly:regex-exec regex "ac")))
   (assert (equal? "" (ly:regex-match-substring m 1)))
   (assert (not (ly:regex-match-substring m 2)))
   (assert (equal? '(1 . 1) (ly:regex-match-positions m 1)))
   (assert (not (ly:regex-match-positions m 2))))

#(let ((regex (ly:make-regex "a")))
   (assert (null? (ly:regex-exec->list regex "bbb")))
   (assert (equal? '((0 . 1) (2 . 3) (4 . 5))
                   (map ly:regex-match-positions
                        (ly:regex-exec->list regex "ababab")))))

#(let ((regex (ly:make-regex "aâ")))
   (assert (equal? '("" "" "bb" "") (ly:regex-split regex "aâaâbbaâ")))
   (assert (equal? '("bƀ" "bƀ") (ly:regex-split regex "bƀaâbƀ")))
   (assert (equal? '("") (ly:regex-split regex ""))))

#(let* ((regex (ly:make-regex "(\\W)(\\w)(\\w)(\\w)"))
        (m (ly:regex-exec regex "àçΣabcẞ+Äaèêù")))
   (assert (equal? "+Äaè" (ly:regex-match-substring m)))
   (assert (equal? '(7 . 11) (ly:regex-match-positions m)))
   (assert (equal? "+" (ly:regex-match-substring m 1)))
   (assert (equal? '(7 . 8) (ly:regex-match-positions m 1)))
   (assert (equal? "Ä" (ly:regex-match-substring m 2)))
   (assert (equal? '(8 . 9) (ly:regex-match-positions m 2)))
   (assert (equal? "a" (ly:regex-match-substring m 3)))
   (assert (equal? '(9 . 10) (ly:regex-match-positions m 3)))
   (assert (equal? "è" (ly:regex-match-substring m 4)))
   (assert (equal? '(10 . 11) (ly:regex-match-positions m 4))))

#(let ((regex (ly:make-regex "\\p{L}")))
   (assert (equal? "+1234¡++1234+"
                   (ly:regex-replace regex "a1234¡Äè1234ù" "+"))))
