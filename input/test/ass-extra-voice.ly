global = \notes {
	\key a \minor;
	\time 6/4;
%	\skip 1.*34;
%	\bar ".|";
}

melody = \notes\relative c''{
	r2 r r 
	r2 r r
	r4 a'8-- \< a--	a-- a-- c-- \!b-- a--\> gis f \!e 
	es8 \grace b c r4 r2 r
}

basloopje = \notes\relative c{
	d,8(	a' d f a d f d a f d	)a
}

accompany = \notes \relative c{
	\notes\relative c \basloopje
	\notes\relative c \basloopje
	\notes\relative c \basloopje
	\notes\relative c \basloopje
}

\score{
	\notes{
		\context AutoSwitchGrandStaff \relative c <
			\global
			\context Staff=upper { 
				\context Voice=foo
				\property Voice.verticalDirection = 1
				\property Voice.scriptVerticalDirection = 1
				\melody 
			}
			\context AutoSwitchContext \accompany
		>
	}

	\paper {
		gourlay_maxmeasures = 4.;
		indent = 8.\mm;
		textheight = 295.\mm;

		% no slur damping
		slur_slope_damping = 100.0;

		\translator{ 
			\StaffContext
			% don't auto-generate bars: not a good idea: -> no breakpoints
			% barAuto = "0";
			% urg defaultBarType = "";
			defaultBarType = "empty";
			\remove "Time_signature_engraver";
		}
		\translator{ 
			\GraceContext
			\remove "Local_key_engraver";
		}
		\translator { 
			\ScoreContext
			\accepts AutoSwitchGrandStaff;
		}
		\translator{
			\type "Engraver_group_engraver";
			\name AutoSwitchGrandStaff;
			\consists "Span_bar_engraver";
			\consists "Vertical_align_engraver";
			\consists "Piano_bar_engraver";
			\consistsend "Axis_group_engraver";
			minVerticalAlign = 2.*\staffheight;
			maxVerticalAlign = 2.*\staffheight;	
			switcherName = "Voice";
			acceptorName = "Thread";
			staffContextName = "Staff";

			\accepts "AutoSwitchContext";
			\accepts "Staff";
			slurVerticalDirection = 1;
			verticalDirection = -1;
			beamAutoEnd = "1/2";
		}
		\translator {
			\type "Engraver_group_engraver";
			\name "AutoSwitchContext";
			\consists "Staff_switching_translator";
		}
	}
% broken 1.1.51.hwn2
%	\midi {
%		\tempo 4 = 54;
%	}
}

