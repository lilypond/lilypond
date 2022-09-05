\version "2.23.13"

\layout {
  system-count = 2
  \context {
    \Score
    \remove Bar_number_engraver
  }
}

%% Genevan Psalter, 1551; attributed to Louis Bourgeois
\score {
  \fixed c' {
    \key f \major
    \time 2/2
    f1^\markup { \italic "Genevan Psalter" rhythm } f2 e d c f1 g a \caesura
    a1 a2 a g f bes1 a g \caesura \break
    f1 g2 a g f d1 e f \caesura
    c' a f g2 bes a1 g f \fine
  }
}

\score {
  \fixed c' {
    \key f \major
    \time 2/2
    \partial 2
    f2^"Simplified rhythm" f e d c f g a \caesura
    a2 a a g f bes a g \caesura \break
    f2 g a g f d e f \caesura
    c'2 a f g bes a g f \fine
  }
}
