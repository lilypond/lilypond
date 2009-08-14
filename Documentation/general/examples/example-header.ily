\version "2.11.33"
\paper{
  indent=0\mm
  ragged-last=##f
  ragged-right=##f
  line-width=158\mm  % produces 624 pixels
  oddFooterMarkup=##f
  oddHeaderMarkup=##f
  bookTitleMarkup = ##f
  scoreTitleMarkup = ##f
}

\layout {
  \context { \Score
    \override PaperColumn #'keep-inside-line = ##t
    \override NonMusicalPaperColumn #'keep-inside-line = ##t
  }
}


