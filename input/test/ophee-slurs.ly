\header{
texidoc="
Slurs can be forced to always attach to note heads.
";
}


\score{
	\notes \relative c''{
		\property Voice.VerticalDirection = #1
		\property Voice.slurBeginAttachment = #'head
		\property Voice.slurEndAttachment = #'head
		g16()g()g()g()d'()d()d()d
	}
	\paper{
		indent = 0.0;
		linewidth = 60.0\mm;
	}
}
