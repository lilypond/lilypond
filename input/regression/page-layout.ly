
\version "2.4.0"


    
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
    %hsize = 2\cm
    %vsize = 2\cm
    topmargin = 2\cm
    bottommargin = 2\cm
    %headsep = 7\cm
    %footsep = 7\cm
    raggedbottom = ##t
    raggedlastbottom = ##t
    leftmargin = 3\cm
    linewidth = 15\cm
    %rigthmargin = 3\cm
    interscoreline = 3\cm
}


\book {
    \score {
        \relative c'' {
            \repeat unfold 10 {
                \repeat unfold 4 {c4}
            }
        }
        \header {piece = "Piece I"}
        \layout {linewidth = 13\cm}
    }
    \score {
        \relative c'' {
            \repeat unfold 10 {
                \repeat unfold 4 {c4}
            }
        }
        \header {piece = "Piece II"}
        \layout {linewidth = 9\cm}
    }
}
