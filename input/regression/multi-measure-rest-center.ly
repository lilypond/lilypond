\version "2.3.17"
\header
{
  texidoc = "The multimeasure rest is centered exactly between bar lines."

}

\score {
    
    {
	\set Score.skipBars = ##t
	\override Staff.BarLine  #'hair-thickness = #7.5
	\override Staff.MultiMeasureRest  #'hair-thickness = #10

	c'1 R1*20 c'1
    }
    \paper { raggedright = ##t} 
}
	 

