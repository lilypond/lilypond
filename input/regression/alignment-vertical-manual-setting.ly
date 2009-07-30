\header {

  texidoc = "Alignments may be changed pre system by setting
  @code{alignment-distances} in the @code{line-break-system-details}
  property"

}

\version "2.13.2"

\book {
  \score {
    \relative c'' \new StaffGroup <<
      \new Staff { c1 c c c }
      \new Staff { c c c c }
      \new Staff { 
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((alignment-distances . (#f 20)))
	c1 \break
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((alignment-distances . (5 15)))
	c1 \break
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((alignment-distances . (15 5)))
	c1 c
      }
    >>
  }
}
