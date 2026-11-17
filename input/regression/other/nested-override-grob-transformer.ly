\version "2.27.6"

\header {
  texidoc = "A sub-property override combined with a whole-property override by
@code{grob-transformer} must complete and apply both, regardless of order.  A
plain Scheme procedure as the base works as well."
}

#(ly:set-option 'warning-as-error #t)

#(define (check-spacing expected . opt)
   ;; Optional argument: if given, check only the unpure value.
   (let ((pure? (null? opt)))
     (lambda (grob)
       (for-each
        (lambda (v)
          (for-each
           (lambda (e)
             (if (not (equal? (assq-ref v (car e)) (cdr e)))
                 (ly:grob-warning grob 'staff-staff-spacing
                                  "~a is ~s, expected ~s"
                                  (car e) (assq-ref v (car e)) (cdr e))))
           expected))
        (cons (ly:grob-property grob 'staff-staff-spacing)
              (if pure?
                  (list (ly:grob-pure-property grob 'staff-staff-spacing 0 1))
                  '())))
       #t)))

addPadding =
#(grob-transformer 'staff-staff-spacing
   (lambda (grob alist) (acons 'padding 3 alist)))

\score {
  <<
    % Transformer older than the sub-override: without the guard entry
    % in the cooked list, this recurses.
    \new Staff \with {
      \override VerticalAxisGroup.staff-staff-spacing = \addPadding
      \override VerticalAxisGroup.staff-staff-spacing.minimum-distance = 11
      \override VerticalAxisGroup.after-line-breaking =
        #(check-spacing '((basic-distance . 9)
                          (minimum-distance . 11)
                          (padding . 3)))
    } { c'1 }

    % Transformer newer than the sub-override.
    \new Staff \with {
      \override VerticalAxisGroup.staff-staff-spacing.minimum-distance = 11
      \override VerticalAxisGroup.staff-staff-spacing = \addPadding
      \override VerticalAxisGroup.after-line-breaking =
        #(check-spacing '((basic-distance . 9)
                          (minimum-distance . 11)
                          (padding . 3)))
    } { c'1 }

    % Plain Scheme procedure as the base (not an unpure-pure-container).
    % Checked in unpure mode only, since a plain procedure has no pure
    % branch.
    \new Staff \with {
      \override VerticalAxisGroup.staff-staff-spacing =
        #(lambda (grob) '((basic-distance . 9) (padding . 1)))
      \override VerticalAxisGroup.staff-staff-spacing.padding = 5
      \override VerticalAxisGroup.after-line-breaking =
        #(check-spacing '((basic-distance . 9) (padding . 5)) 'unpure-only)
    } { c'1 }
  >>
}
