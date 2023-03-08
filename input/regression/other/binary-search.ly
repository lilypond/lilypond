\version "2.25.3"

\header {
  texidoc = "The @code{binary-search} function works as documented."
}

% Basic usage
#(assert (eqv? 3 (binary-search 0 5 identity 3)))
% No integer reaches exactly the target value
#(assert (eqv? 3 (binary-search 0 5 (lambda (x) (* 2 (floor-quotient x 2))) 3)))
% Several integers reach the target value
#(assert (eqv? 5 (binary-search 0 5 (lambda (x) (* 2 (floor-quotient x 2))) 4
                  ;; this is the default
                  #:mode 'last-less-than-or-equal)))

#(assert (eqv? 3 (binary-search 0 5 identity 3 #:mode 'first-greater-than-or-equal)))
#(assert (eqv? 4 (binary-search 0 5 (lambda (x) (* 2 (floor-quotient x 2))) 3
                  #:mode 'first-greater-than-or-equal)))
#(assert (eqv? 4 (binary-search 0 5 (lambda (x) (* 2 (floor-quotient x 2))) 4
                  #:mode 'first-greater-than-or-equal)))
