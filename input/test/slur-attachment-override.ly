\header{
texidoc="
In some cases, you may want to set slur attachments by hand.
";
}

\score{
	\notes \relative c''{
		\property Voice.Stem \set #'length = #5.5
		\property Voice.Slur \set #'direction = #1
		\property Voice.Slur \set #'attachment = #'(stem . stem)
		g8(g)g4
		g4(g8)g
	}
	\paper{
		indent = 0.0;
		linewidth = 60.\mm;
	}
}
