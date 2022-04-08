\header {

  texidoc = "By setting @code{Y-offset} and @code{X-offset} for the
@code{line-break-system-details} of @code{NonMusicalPaperColumn},
systems may be placed absolutely on the printable area of the page."

  tagline = "this is the tagline"

}

\version "2.23.8"

#(set-default-paper-size "a6" 'portrait)

\paper {
  line-width = 5.0\cm
  headerMarkup = "header"
  indent =0.0
  annotate-spacing = ##f
  annotate-page = ##t
  annotate-headers = ##t
}

\book {
  \score {
    {
      \once \override
      Score.NonMusicalPaperColumn.line-break-system-details =
      #'((Y-offset . 0.0))
      c1 c1 \break
      
    
      \once \override
      Score.NonMusicalPaperColumn.line-break-system-details =
      #'((Y-offset . 62.0)
         (X-offset . 8.0)
      )
      c1 c1
    }
  }
}
