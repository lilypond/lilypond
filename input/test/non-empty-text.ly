%
% Try this file with 1.2.17: it works
% 1.2 had a different default, and uses textEmptyDimension scalar property
%
\score{
	\notes\relative c''{
		%\property Staff.textEmptyDimension=1
		a-"This text has no"
		a
		a
		a-"width; the default"
		\break
		\property Staff.textNonEmpty=##t
		%\property Staff.textEmptyDimension=0
		a-"This text"
		a-"is fat: notes are spaced"
		a-"far apart and text"
		a-"does not collide"
		}
	  \paper{
	      linewidth=80.\mm;
	  }
}
