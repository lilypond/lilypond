\version "1.3.96";
	
\score{
	\notes\relative c''{
		\context Staff \property Staff.TextScript \pop #'no-spacing-rods
		\context Staff \property Staff.TextScript \push #'no-spacing-rods = ##f
		c4_"longlonglonglonglonglong" c4_"text" 
		}
	  \paper{
	      linewidth=-80.\mm;
	  }
}
