\header
{
  texidoc = "The multimeasure rest is centered exactly between bar lines."

}

\score {
    \notes
    {
	\property Score.skipBars = ##t
	\property Staff.BarLine \set #'hair-thickness = #7.5
	\property Staff.MultiMeasureRest \set #'hair-thickness = #10

	c'1 R1*20 c'1
    }
    \paper { linewidth = -1.0 } 
}
	 
