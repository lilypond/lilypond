\version "1.3.146"
% Example of figured bass, using text scripts.
% (An alternative is to use a lyrics line if you want the figures
% aligned vertically.)



% Scheme macros for accidentals. Note how they can be combined
% with other strings, for example in: d^#`(columns ,sharp "4")

#(define sharp '((raise . 0.2) (music (named "accidentals-1"))))
#(define natural '((raise . 0.2) (music (named "accidentals-0"))))
#(define flat '((raise . 0.2) (music (named "accidentals--1"))))


\score{
  \notes \relative c'{
    \clef bass

      c^"5" d^#natural g,^"7 6" [a8 e] |
      fis4^"7 6" [g8 d] e4^"7 6" [f?8 c] |
      [d^#sharp d b g][c^"7" c^"5" a^"6" f] |
      [bes^"7" bes^"5" g^"6" e] a4^#sharp d^"6" ~ |
      d^#`(columns ,sharp "4") c^"6" d e^#sharp |
  }
}

