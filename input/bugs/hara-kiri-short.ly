\header{
texidoc="
Hara kiri staffs kill themselves if they are empty.  This example really
contains two staffs, but the second contains only spaces, and is
therefore removed.  Also, any staff brackets and braces are removed.
";
}

\version "1.3.117";
zager =  \context Staff = zager \notes \relative c'' {
	\clef treble;
	c1
}

zoger =  \context Staff = zoger \notes \relative c'' {
	\clef treble;
	\skip 1* 1;
}

zagers =  \context GrandStaff <
	\zager
	\zoger
>
 
\score{
	<
		\context StaffGroup = zagers <
			\zagers
		>
	>
	\paper{
		linewidth = 80.0\mm;

		\translator { \HaraKiriStaffContext }
%uh?

	}
}


