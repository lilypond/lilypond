\version "1.5.68"
\header{
texidoc="
Hara kiri staves kill themselves if they are empty.  This example really
contains two staves, but the second contains only spaces, and is
therefore removed.  Also, any staff brackets and braces are removed.
"
}





zager =  \context Staff = zager \notes \relative c'' {
	\clef treble
	c1	c1
}

zeuger =  \context Staff = zeuger \notes \relative c'' {
	\clef treble
	c1	c1
}

zoger =  \context Staff = zoger \notes \relative c'' {
	\clef treble
	\skip 2* 1
}

zagers =  \context StaffGroup <
	\zager
	\zoger
	\zeuger
>
 
\score{
	< \context StaffGroup = zagers <
			\zagers
	> >
	\paper{
		linewidth = 80.0\mm

		\translator { \HaraKiriStaffContext }
%uh?

	}
}


