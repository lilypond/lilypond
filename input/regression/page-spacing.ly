
\header {

  texidoc = "By setting properties in NonMusicalPaperColumn, vertical
spacing of page layout can be adjusted.

For technical reasons, @code{overrideProperty} has to be used for
setting properties on individual object. @code{\override} may still be
used for global overrides.

By setting @code{annotate-spacing}, we can see the effect of each property.
"

}

\version "2.12.0"

#(set-global-staff-size 11)

\book {
  \score {
    \relative c'' \new PianoStaff <<
      \new Voice  {
	c1_"followed by default spacing"\break
	c\break

	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((Y-extent . (-30 . 10)))
	c_"Big bounding box (property Y-extent)"\break

	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((refpoint-Y-extent . (-37 . -10)))
	c_\markup {
	  \column {
	    "Refpoints further apart (property refpoint-Y-extent)."
	    "Stretchable space runs between refpoints"
	  }
	}
	
	\break
	
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((next-padding . 10))

	c_"Followed by padding, ie unstretchable space. (property next-padding)" \break
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((next-space . 20))
	c_"Followed by stretchable space (property next-space)"\break
	c\break
	\overrideProperty
	#"Score.NonMusicalPaperColumn" #'line-break-system-details
	#'((bottom-space . 25.0)) 
	c_"25 staff space to the bottom of the page. (property bottom-space)"\break


      }
      { c1 c c c c c c c }
    >>
  }
  \paper {
    ragged-last-bottom = ##f
    annotate-spacing = ##t
    between-system-space = 1.0
    #(set! text-font-defaults
      (acons
       'font-size 6
       text-font-defaults)
      
    )
  }
}
