\header{
texidoc=3D"Real header: the slurBeginAttachment setting still lives in the
second example, breaking the cross staff broken slur.
";
}
=09
\header{
texidoc=3D"
Slurs can be forced to always attach to note heads.
";
}


\score{
	\notes \relative c''{
		\property Voice.VerticalDirection =3D #1
		\property Voice.slurBeginAttachment =3D #'head
		\property Voice.slurEndAttachment =3D #'head
		g16()g()g()g()d'()d()d()d
	}
	\paper{
		indent =3D 0.0;
		linewidth =3D 60.0\mm;
	}
}
=09
\header{
texidoc=3D"
The same goes for slurs. They behave decently when broken across
linebreak.
";
}

\score{
	\context PianoStaff <
	\context Staff=3Done \notes\relative c'{
%{
		\stemUp \slurUp
		 c4( c \translator Staff=3Dtwo c )c |
		\translator Staff=3Done
		\stemUp \slurUp
		 c4( c \translator Staff=3Dtwo c )c |
		\stemUp \slurUp
		 c4( c \translator Staff=3Done c )c |
		\translator Staff=3Dtwo
		\stemUp \slurUp
		 c4( c \translator Staff=3Done c )c |
		\translator Staff=3Dtwo
		\stemUp \slurUp
		 c4( \translator Staff=3Done c c )c |
%}
		r2
		\translator Staff=3Dtwo
		\stemUp \slurUp
		 c4( \translator Staff=3Done c
		   \break
		c )c
		r2

%{
		\stemDown \slurDown
		 d4( \translator Staff=3Dtwo c c \translator Staff=3Done )d
		\translator Staff=3Dtwo
		\stemUp \slurUp
		 c4( \translator Staff=3Done c c \translator Staff=3Dtwo )c
		r1
%}
	}
	\context Staff=3Dtwo \notes\relative c'{
		\clef bass;
		s1 s1 %s1 s1 s1 s1 s1 s1 s1 s1
	}
	>
	\paper { indent =3D 0.; linewidth =3D 40.*\staffspace; }
}

\version "1.3.110";=20
