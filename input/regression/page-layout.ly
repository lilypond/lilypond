
\version "2.3.16"

\header{ texidoc = "This shows how different settings on \bookpaper
modify the general page layout. Basically \bookpaper will set the
values for the whole paper while \paper for each \score block." }

 \header {
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

\bookpaper {
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
        \paper {linewidth = 13\cm}
    }
    \score {
        \relative c'' {
            \repeat unfold 10 {
                \repeat unfold 4 {c4}
            }
        }
        \header {piece = "Piece II"}
        \paper {linewidth = 9\cm}
    }
}
