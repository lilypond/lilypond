\version "2.1.30"
\header
{
  texidoc = "The multimeasure rest is centered exactly between bar lines."

}

\score {
    \notes
    {
	\set Score.skipBars = ##t
	\override Staff.BarLine  #'hair-thickness = #7.5
	\override Staff.MultiMeasureRest  #'hair-thickness = #10

	c'1 R1*20 c'1
    }
    \paper { raggedright = ##t} 
}
	 

