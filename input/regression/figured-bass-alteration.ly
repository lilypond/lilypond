\version "2.23.4"

\header {
  texidoc = "Bass figures can carry alterations, which may also be
bracketed."
}

\layout {
  ragged-last = ##f
  indent = 0
}

#(for-each
  (lambda (i) (ly:expect-warning "no accidental glyph found"))
  (iota 6))

<<
  \new Staff {
    \time 5/4
    \clef bass
    \repeat unfold 20 d4
  }
  \figures
  {
    % 1) Ordinary accidentals
    <3--> <3-> <3!> <3+> <3++>

    % 2) Bracketed accidentals
    <3[--]> <3[-]> <3[!]> <3[+]> <3[++]>

    % 3) Multi-number figures, figure _, combination with bass
    %    figure brackets (also, whitespace inside < > is ignored.)
    <6 [ + ]3[  - ] >
    <[6[+]
    4[-]]
    [3   [!]]>
    <6 _ [ + ] >
    <  [ 6 _ [ -   ]   ] >

    % 4) Not existing accidentals cause a warning and are replaced by "?".
    <5+++> <[5+++]>  <5[+++]> <5---> <[5---]>  <5[---]>
  }
>>
