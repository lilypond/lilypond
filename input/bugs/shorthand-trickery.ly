%
% Example of shorthand trickery 
% \property Context.foo = bar
%
foo = \property Staff.foo = #"bar"
%
%
%

one = \context Voice\notes\relative c {
	a1 b c
}


two = \context Voice\notes {
	a1\foo
	b
	< c\foo e >
}

\score{
	\context OtherStaff=one <
		\one
		\two
	>
	\paper{
		\translator {
			\StaffContext
			\name "OtherStaff";
		}
		\translator {
			\ScoreContext
			\accepts "OtherStaff";
		}
	}
}
