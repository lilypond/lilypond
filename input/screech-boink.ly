
clusterSneeuwbal =\notes {
		\property Thread.NoteHead \set #'transparent = ##t
		\property Voice.Stem \set #'transparent = ##t
		\property Voice.Beam \set #'transparent = ##t
%		\property Staff.Accidental \set #'transparent = ##t
		\property Voice.Cluster \set #'padding = #0.25
		\property Voice.Cluster \set #'shape = #'ramp
}
\score {
\notes \context PianoStaff <
  \context Staff = up {
	\time 4/8
    \key c \minor


< { \property Voice.Stem \override #'direction = #'()   	\translator Staff = down
    \property Voice.subdivideBeams = ##t	     
 	g16.-[
	\translator Staff = up
	c'''32 	\translator Staff = down
	g32 \translator Staff = up
	c'''32	\translator Staff = down
	 g16-]
	\translator Staff = up
\property Voice.Stem \revert #'direction
	\property Voice.followVoice = ##t
	c'''32-(-[ b''16 a''16 gis''16 g''32-)-]  } \\
	{ s4 \times 2/3 { d'16-[ f' g'-] } as'32-[ b''32 e'' d''-] } \\
	{ s4 \autoBeamOff d''8.. f''32  } \\
	{ s4 es''4 }
	>
  }

  \context Staff = down {
	\clef bass
	\key c \minor
	\property Voice.subdivideBeams = ##f
    \property Voice.Stem \set #'french-beaming =  ##t
\property Voice.Beam \set #'thickness = #0.3
\property Voice.Stem \set #'thickness = #4.0
    [g'16 b16 fis16 g16]
< { \clusterSneeuwbal
    as16-\startCluster <<as b>>
    <<g b>>
    <<g cis>>-\stopCluster
  } \\
 {
\property Staff.Arpeggio \set #'arpeggio-direction =#-1
<<cis, e, gis, b, cis>>4-\arpeggio  }
  >
 }
>

\paper { linewidth = -1.0

	\translator { \StaffContext \consists Horizontal_bracket_engraver }
}
\midi { \tempo 8 = 60 }
}
