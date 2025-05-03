\version "2.25.27"

\header {
  texidoc = "Three lines with: default ledgers, ledgers from
@code{NoteHead.ledger-positions} and from
@code{StaffSymbol.ledger-positions-function}.  The latter two internally use
@code{ledger-lines::positions-from-ledgered-grob}.  All three lines should print
identically."
}

#(define note-head::scheme-ledgers
  (lambda (grob)
    (let* ((staff-pos (ly:grob-property grob 'staff-position))
           (staff-symbol (ly:grob-object grob 'staff-symbol))
           (staff-symbol-ledger-positions
             (ly:grob-property staff-symbol 'ledger-positions '(0 2))))
      (if (pair? staff-symbol-ledger-positions)
          (let ((l-lines
                  (ledger-lines::positions-from-ledgered-grob
                    staff-symbol staff-pos))
                (staff-symbol-line-positions
                  (ly:grob-property staff-symbol 'line-positions)))

            ;; Remove ledger-line-positions matching staff-line-positions.
            (lset-difference
              =
              l-lines
              staff-symbol-line-positions))
          '()))))

%% There are infinite possibilities combining 'ledger-extra, 'line-positions and
%% 'ledger-positions. Here are some examples.
#(define ledger-pattern
;  '(0 (2 3 4))
;  '(0 1)
;  '(0 (2 3 4) 6)
;  '(0 (2 3 4) 6 (8 9 10))
;  '(2 (4 5 6))
;  '(0 (3 4 5))
;  '(4 (6 7 8) 10)
;  '(6 (8 9 10))
;  '(8 (10 11 12))
;  '(10 (12 13 14) 16)
  '(20 (22 23 24) 26)
;  '(0 2 4 6 8 10 12 14)
 )

#(define extra-ledgers -5)

#(define staff-lines
  '(-19 -7 7 19)
;  '(-20 -10 0 10 20)
;  '(0)
  )

testNotes = {
  c, d, e, f, g, a, b, c d e f g a b
  c' d' e' f' g' a' b' c'' d'' e'' f'' g'' a'' b''
  c''' d''' e''' f''' g''' a''' b''' c'''' d'''' e'''' f'''' g'''' a'''' b''''
  c'''''
}

lpf =
 \override Staff.StaffSymbol.ledger-positions-function =
   #'(lambda (grob pos)
       (lset-difference
         =
         (ledger-lines::positions-from-ledgered-grob grob pos)
         (ly:grob-property grob 'line-positions)))


m = {
  \textMark
    \markup { Default, ledger-extra: $(number->string extra-ledgers) }

  \testNotes
  \bar "." \break

  \textMark
    \markup { Scheme code, ledger-extra: $(number->string extra-ledgers) }
  \override NoteHead.ledger-positions = #note-head::scheme-ledgers
  \testNotes
  \revert NoteHead.ledger-positions
  \stopStaff \startStaff
  \bar "." \break

  \textMark
    \markup
       {
         ledger-positions-function, scheme code,
         ledger-extra: $(number->string extra-ledgers)
       }
  \lpf
  \testNotes
}

\score {
  { \cadenzaOn \m \bar "||" }
  \layout {
    indent = 0
    \context {
      \Staff
      \override StaffSymbol.line-positions = #staff-lines
      \override StaffSymbol.ledger-positions = #ledger-pattern
      \override StaffSymbol.ledger-extra = #extra-ledgers
      \omit TimeSignature
    }
  }
}
