\version "1.3.117";
\paper{
	%my standard paper block

	% figure out a way to do this cleanly.

%	0=\font "feta19"
%	-1=\font "feta16"
%	\stylesheet #(make-style-sheet 'paper19)

	arithmetic_multiplier=7.\pt;
	indent=0.;
	linewidth=188.\mm;
	forced_stem_shorten0=0.;
	forced_stem_shorten1=0.;
	forced_stem_shorten2=0.;
	forced_stem_shorten3=0.;
	\translator{
		\PianoStaffContext
		maxVerticalAlign=8.4;
		minVerticalAlign=8.4;
	}
	\translator{\StaffContext
		TimeSignature \override #'style = #"C"
	}
	\translator{\VoiceContext
		noStemExtend = ##t
		tupletVisibility = ##f
	}
}


