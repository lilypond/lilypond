\header {

  texidoc = "Alignments may be changed pre system by setting
  @code{alignment-offsets} in the @code{line-break-system-details}
  property"

}

\version "2.12.0"

\book {
  \score {
    \relative c'' \new StaffGroup <<
      \new Staff { c1 c c c }
      \new Staff { c c c c }
      \new Staff { 
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((alignment-offsets . (#f #f -30)))
	c1 \break
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((alignment-offsets . (0 -5 -20)))
	c1 \break
	\overrideProperty
	#"Score.NonMusicalPaperColumn"
	#'line-break-system-details
	#'((alignment-offsets . (0 -15 -20)))
	c1 c
      }
    >>
  }
}
