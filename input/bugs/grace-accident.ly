%grace-accident.ly
\score{
	\context Staff=foo \notes\relative c''{
%		c8 % remove line to dump core
		\grace { cis8 dis } e,4
	}
}
