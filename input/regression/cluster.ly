\version "1.7.16"
\header {
    texidoc = "Clusters are a device to denote that a complete range of
notes is to be played."
}

voiceI = % same as voiceII, but with ordinary notes
	\context Voice = voiceI {
	    \notes \relative c' {
		\stemUp
		c4 f4
		a4 <<e d'>>4 | \break
		<< g a >>8 << e a >>8 a4 c1 << d b >>4 e4 |
		c4 a4 f4 g4 a4
	    }
	}

voiceII = % same as voiceI, but with cluster notation
	\context Voice = voiceII {
	    \notes \relative c' {
		\property Thread.NoteHead \set #'transparent = ##t
		\property Voice.Stem \set #'transparent = ##t
		\property Voice.Beam \set #'transparent = ##t
		\property Staff.Accidental \set #'transparent = ##t
		\property Voice.Cluster \set #'padding = #0.25
		\property Voice.Cluster \set #'shape = #'ramp
		c4 f4-\startCluster
		a4 <<e d'>>4 | \break
		%%% do not try something like: < { g8 e8 } a4 >
		%%% instead, do the following: << g a >>8 << e a >>8
		<< g a >>8 << e a >>8 a4 c1 << d b >>4 e4 |
		c4-\stopCluster a4 f4 g4 a4
	    }
	}

\score {
	\context PianoStaff {
	<
		\voiceI
		\voiceII
	>
	}
	\paper{
		linewidth = 15.0 \cm
	}
}
%% new-chords-done %%
