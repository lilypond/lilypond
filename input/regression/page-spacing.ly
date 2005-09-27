
\header {

  texidoc = "By setting properties in NonMusicalPaperColumn, vertical
spacing of page layout can be adjusted.

For technical reasons, @code{outputProperty} has to be used for
setting properties on individual object. @code{\override} may still be
used for global overrides.

"

}

\version "2.7.10"

#(set-global-staff-size 11)

\book {
  \score {
    \relative c'' \new StaffGroup <<
      \new Voice  {
	c1\break

	\outputProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((Y-extent . (-30 . 10)))
	c^"This system has big extents (property Y-extent)"\break

	c\break
	\outputProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((next-padding . 20))

	c^"This system is followed by padding, ie unstretchable space. (property next-padding)" \break
	\outputProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((next-space . 20))
	c^"This system is followed by stretchable space (property next-space)"\break
	c\break
	c\break
	\outputProperty
	#"Score.NonMusicalPaperColumn" #'line-break-system-details
	#'((bottom-space . 25.0)) 
	c^"This system has 25 staff space to the bottom of the page. (property bottom-space)"\break


      }
      { c1 c c c c c c c }
    >>
  }
  \paper {
    raggedlastbottom = ##f
    betweensystemspace = 1.0
  }
}
