\version "2.19.21"

\header {
  texidoc = "The page-turn engraver will not count potential page
turns if they occur in the middle of a repeat unless there is a
long gap at the beginning or at the end of the repeat.
"
}

\layout {
  \context {
    \Staff
    \consists "Page_turn_engraver"
  }
}

\book {
  \paper {
    #(define page-breaking ly:page-turn-breaking)
    paper-height = #80
    print-page-number = ##t
    print-first-page-number = ##t
  }

  \score {
    \relative {
      \set Score.skipBars = ##t
      % this should be kept on one page
      \repeat volta 2 {
	\repeat unfold 6 {a4 b c d16 d d d} R1*10
	\repeat unfold 8 {a4 b c d16 d d d} \pageTurn
      }
      % use up a page
      a4 b c d a b c d \pageBreak

      % this should be allowed to have a page turn
      \repeat volta 2 {
	\repeat unfold 7 {a4 b c d16 d d d} R1*10
	\repeat unfold 7 {a4 b c d16 d d d} R1*3
      }
    }
  }
}
