%
% Try this file with 1.2.17: it works
% 1.2 had a different default, and uses textEmptyDimension scalar property
%
\score{
	\notes\relative c''{
		\property Staff.textNonEmpty=##t
		c4_"longlonglonglonglonglong" c4_"text" 
		}
	  \paper{
	      linewidth=-80.\mm;
	  }
}
