\score{
	\notes \relative c''{
		% URG, make stem length match beam!
		\property Voice.stemLength = #5

		\property Voice.slurVerticalDirection = #1
		\property Voice.slurEndAttachment = #'stem
		a8(a)a4
		\property Voice.slurEndAttachment = ##f
		\property Voice.slurBeginAttachment = #'stem
		a4(a8)a
	}
	\paper{
		indent = 0.0;
		linewidth = 60.0\mm;
	}
}
