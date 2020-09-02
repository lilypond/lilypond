
\version "2.19.21"



\header {

     texidoc = "This shows how different settings on \paper modify the
general page layout. Basically \paper will set the values for the
whole paper while \layout for each \score block.

This file is best viewed outside the collated files document.
"

    title = "Title"
    subtitle = "(and (the) subtitle)"
    subsubtitle = "Sub sub title"
    poet = "Poet"
    composer = "Composer"
    texttranslator = "Text Translator"
    meter = "Meter"
    arranger = "Arranger"
    instrument = "Instrument"
  }


\paper {
    %paper-width = 2\cm
    %paper-height = 2\cm
    top-margin = 2\cm
    bottom-margin = 2\cm
    ragged-bottom = ##t
    ragged-last-bottom = ##t
    left-margin = 3\cm
    line-width = 15\cm
    %rigthmargin = 3\cm

    annotate-spacing = ##t
  }


\book {
    \score {
        \relative {
            \repeat unfold 10 {
                \repeat unfold 4 {c''4}
            }
        }
        \header {piece = "Piece I"}
        \layout {line-width = 13\cm}
    }
    \score {
        \relative {
            \repeat unfold 10 {
                \repeat unfold 4 {c''4}
            }
        }
        \header {
	  breakbefore = ##f
	  piece = "Piece II"
	}
        \layout {line-width = 9\cm}
    }
}
