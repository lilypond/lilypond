\header{
Composer="Sergej Rachmaninov (1873-1943)";
Title="Elegie";
Subtitle="À Monsieur A. Arensky";
Opus="Opus 3 Nr 1";
Note="Small fragment to show Pedal and Dynamics problems";
Enteredby="JCN";
}

%
% Arg.  
% How to remove dynamics from Voice/Staff, but still show fingering?
% When Dynamic_engraver is removed, the Text_engraver happily engraves
% the dynamic markings.
% When the Text_engraver is removed, it can't engrave the fingerings.
%

global = \notes{
	\key es \minor;
	\time 4/4;
}

upperMusic = \context Voice\notes\relative c'' {
	%1
	R1
	R1
	r2 ges'2--~
}

upperDynamics = \context Voice\notes {
	%1
	s1\pp
	s1
	s2 s2\mf
}

upperFingering = \context Voice\notes {
	%1
	s1
	s1
	s2 s2-4
}

lowerMusic = \context Voice\notes\relative c {
	%1
	<es,8(es,> bes' ges' es' bes' es, ges,)bes,
	<es,8(es,> bes' ges' es' bes' es, ges,)bes,
	<es,8(es,> bes' ges' es' bes' es, ges,)bes,
}

lowerPedal = \context Voice\notes {
	%1
	%1
	s8\unachorda\sustaindown s8 s4 s2
	s8\sustaindown\sustainup s8 s4 s2
	s8\sustaindown\sustainup s8 s4 s2\trechorde
}

lowerDynamics = \context Voice\notes {
	%1
}

lowerFingering = \context Voice\notes {
	%1
	s8 s8-5 s-2 s-1 s-2 s-1 s-2 s
	s1
	s1
}

\score{
	\context PianoStaff <
		\context UpperStaff=upper <
			\global
			\upperMusic
			\upperDynamics
			\upperFingering
			\lowerPedal
 		>
		\context LowerStaff=lower <
			\global
			\clef bass;
			\lowerMusic
			\lowerPedal
			\lowerDynamics
			\lowerFingering
			\upperDynamics
 		>
	>
	\paper{
		% Hmm
		\translator {
			\VoiceContext
			\remove "Piano_pedal_engraver";
			\remove "Dynamic_engraver";
			% AAAARGH!
			\remove "Text_engraver";
		}
		\translator {
			\StaffContext
			\name "UpperStaff";
			\consists "Dynamic_engraver";
			dynamicPadding = #3  % urg, in \pt
			dynamicMinimumSpace = #6  % urg, in \pt
		}
		\translator {
			\StaffContext
			\name "LowerStaff";
			\consists "Piano_pedal_engraver";
			startSustain = #"Ped."
			stopSustain = #"*"
			stopStartSustain = #"-P"
			startUnaChorda = #"una chorda"
			stopUnaChorda = #"tre chorde"
			textScriptPadding = #3.0
		}
		\translator {
			\PianoStaffContext
			\accepts "UpperStaff";
			\accepts "LowerStaff";
		}
		\translator {
			\ScoreContext
			timeSignatureStyle = #"C"
		}
	}
	\midi{
		% Zucht
		\translator {
			\StaffContext
			\name "UpperStaff";
		}
		\translator {
			\StaffContext
			\name "LowerStaff";
		}
		\translator {
			\ScoreContext
			\accepts "UpperStaff";
			\accepts "LowerStaff";
		}
		\tempo 4 = 88;
	}
}

