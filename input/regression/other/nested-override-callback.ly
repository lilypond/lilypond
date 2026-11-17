\version "2.27.6"

\header {
  texidoc = "A sub-property override of a callback-valued property such as
@code{VerticalAxisGroup.staff-staff-spacing} must be applied to the result of
the callback, leaving the other sub-properties intact.  Failures are reported
as warnings."
}

#(ly:set-option 'warning-as-error #t)

#(define (check-spacing expected . opt)
   ;; EXPECTED: alist; a value of #f means "key must be absent".
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

\score {
  <<
    % Grouped: the lower staff of the PianoStaff gets
    % StaffGrouper.staffgroup-staff-spacing, with only `padding' changed.
    \new PianoStaff <<
      \new Staff { c'1 }
      \new Staff \with {
        \override VerticalAxisGroup.staff-staff-spacing.padding = 5
        \override VerticalAxisGroup.after-line-breaking =
          #(check-spacing '((basic-distance . 10.5)
                            (minimum-distance . 8)
                            (padding . 5)
                            (stretchability . 9)))
      } { c'1 }
    >>

    \new StaffGroup <<
      % Ungrouped branch is covered below; this staff only needs to
      % exist as a neighbour below the piano staff.
      \new Staff { c'1 }
    >>
  >>
}

\score {
  <<
    % Ungrouped (default-staff-staff-spacing branch), with stacking:
    % same key twice (newer wins) plus a different key.
    \new Staff \with {
      \override VerticalAxisGroup.staff-staff-spacing.padding = 5
      \override VerticalAxisGroup.staff-staff-spacing.padding = 7
      \override VerticalAxisGroup.staff-staff-spacing.minimum-distance = 11
      \override VerticalAxisGroup.after-line-breaking =
        #(check-spacing '((basic-distance . 9)
                          (minimum-distance . 11)
                          (padding . 7)
                          (stretchability . #f)))
    } { c'1 }

    % Untouched neighbour: must still see the pristine defaults.
    \new Staff \with {
      \override VerticalAxisGroup.after-line-breaking =
        #(check-spacing '((basic-distance . 9)
                          (minimum-distance . 8)
                          (padding . 1)
                          (stretchability . #f)))
    } { c'1 }
  >>
}
