\version "2.17.6"

\header {
  texidoc = "Stems with overridden 'Y-extent should
not confuse height estimation.  This example should fit snugly
on one page.
"
}

#(define (assert-single-page layout props arg)
   (if (and (= (chain-assoc-get 'page:page-number props -1)
               (ly:output-def-lookup layout 'first-page-number))
            (chain-assoc-get 'page:last? props -1))
       (interpret-markup layout props arg)
       (ly:error "failed to fit test on single page")))

\paper {
  #(set-paper-size "a6")
  tagline = ##f
  indent = #0
  system-system-spacing = #'((padding . 1.2))
  oddHeaderMarkup = \markup \on-the-fly #assert-single-page \null
}

\book {
  \score {
    \new Voice {
      \voiceTwo
      \override Stem.Y-extent = #'(0.0 . 0.0)
      \repeat unfold 144 a4
    }
    \layout {
      \context {
        \Score
        \remove "Bar_number_engraver"
      }
    }
  }
}
