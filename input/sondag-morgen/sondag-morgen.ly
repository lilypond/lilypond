\include "paper13.ly"

mu = {}
ac = \accent
su = \translator Staff = up
sd = \translator Staff = down
\include "sondag-morgen-lyrics.ly"
\include "sondag-morgen-folded.ly"

\include "deutsch.ly"
\header {
    title = "Søndag Morgen"
    composer = "Van Morrison (Moon Dance)\\\\Rune Zedeler 2002.06.20"
    piece = "Score"
    tagline = "\\Large{Form: Intro Vers$_1$ Vers$_2$ Omkvæd Vers$_1$ Omkvæd Omkvæd Outro  }"
}
\score {
    \notes <
	\property Score.automaticMelismata = ##t
	\property Score.TimeSignature \override #'style = #'()
	\context Lyrics=marks { \marks }
	\context Lyrics = akk \akk
	\context StaffGroup = melo <
	    \context Lyrics = korlyr { "" }
	    \context Staff=mel <
		\clef "G_8"
		\property Staff.instrument = #'(lines "Kor" "Lead")
		\property Staff.instr = "voc"
		{ s1*8 \break }
		\global
		<
		    \addlyrics
		    \context Voice = kor <
			\voiceOne
			\property Voice.MultiMeasureRest \override #'transparent = ##t
			\transpose c \kormel
		    >
		    \context Lyrics=korlyr \korlyr
		>
		<
		    \addlyrics
		    \context Voice = mel <
			\voiceTwo
			\transpose c \mel
		    >
		    \context Lyrics=lyr \lyr
		>

	    >
	>
	\context StaffGroup = sax <
	    \context Staff = sax <
		\property Staff.instrument = #'(lines "Sop. sax" "Alt sax" "Ten. sax")
		\property Staff.instr = "sax"
		\clef "G"
		\global
		\context Voice = sop < \voiceOne \sopsax >
		\context Voice = alt < \voiceFour \altsax >
		\context Voice = ten < \voiceTwo \tensax >
	    >
	>
	\context StaffGroup = akk <
	    \context PianoStaff = pia <
		\property GrandStaff.instrument = "Piano    "
		\property GrandStaff.instr = "pia   "
		\context Staff = up <
		    \clef "G"
		    \global
		    \context Voice \piar
		>
		\context Staff = down <
		    \global
		    \context Voice \pial
		>
		
	    >
	    \context TabStaff=gui <
		\property Staff.instrument = "Guitar    "
		\property Staff.instr = "gt    "
		\context TabVoice \guitar
	    >
	    \context Staff = cello <
		\property Staff.instrument = "Cello"
		\property Staff.instr = "cel"
		\clef "F"
		\global
		\context Voice \cello
	    >
	    \context Staff = bas <
		\property Staff.instrument = "Bas"
		\property Staff.instr = "bas"

		\clef "F_8"
		\global
		\transpose c \bas
	    >
	    \context Staff = drums <
		\property Staff.instrument = "Trommer"
		\property Staff.instr = "tr"
		\clef "percussion"
		\global
		\apply #(drums->paper 'drums) <
		    \context Voice=hi { \voiceOne \drhi }
		    \context Voice=lo { \voiceTwo \drlo }
		>
	    >
	>
    >
    \paper {
	% linewidth = 26.5\cm
	% textheight = 38.0\cm
	linewidth = 18.8\cm
	indent = 1.0\cm
	\translator {
	    \TabStaffContext
	    \remove "Key_engraver"
	    \consists "Rest_engraver"
	}
	\translator {
	    \GrandStaffContext
	    \accepts "TabStaff"
	    \consists "Instrument_name_engraver"
	}
	\translator {
	    \ScoreContext
	    BarNumber \override #'padding = #2.0
	}
	\translator {
	    \StaffContext
	    minimumVerticalExtent = #'(-4.0 . 4.0)
	    \remove "Piano_pedal_engraver"
	}
 	\translator {
	    \PianoStaffContext
	    VerticalAlignment \override #'forced-distance = #10
	}
   }
   
}


mu = \p
ac = \f
su = {}
sd = {}
\include "sondag-morgen-unfolded.ly"
\include "deutsch.ly"

\score {
    \apply #unfold-repeats
    \notes <
	\context Staff = lead <
	    \property Staff.instrument = "drawbar organ"
	    \context Voice < \mel s1\ff >
	>
	\context Staff = sopsax <
	    \property Staff.instrument = "soprano sax"
	    \sopsax
	>
	\context Staff = altsax <
	    \property Staff.instrument = "alto sax"
	    \altsax
	>
	\context Staff = tensax <
	    \property Staff.instrument = "tenor sax"
	    \tensax
	>
	\context Staff = pia <
	    \property Staff.instrument = "electric grand"
	    \piar \pial
	>
	\context Staff = gt <
	    \property Staff.instrument = "electric guitar (clean)"
	    \guitar
	>
	\context Staff = cel <
	    \property Staff.instrument = "cello"
	    \cello
	>
	\context Staff = bas <
	    \property Staff.instrument = "electric bass (finger)"
	    < \transpose c \bas s1-\ff >
	>
	\context Staff = trom <
	    \property Staff.instrument = "drums"
	    \drhi \drlo
	>


    >
    \midi { \tempo 4 = 128 }
}

