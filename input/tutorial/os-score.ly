
\version "1.3.138"

\include "os-music.ly"
\include "paper13.ly"

#(set! point-and-click line-column-location)
#(define text-flat '((font-relative-size . -2) (music "accidentals--1")))

\score {
  <
    \global
    \property Score.BarNumber \override #'padding = #3
    \context StaffGroup = woodwind <
      \context Staff = flauti <
	\property Staff.midiInstrument = #"flute"
	\property Staff.instrument = "2 Flauti"
	\property Staff.instr = "Fl."
        \Key
	\context Voice=one { \voiceOne \flautoI }
	\context Voice=two { \voiceTwo \flautoII }
      >
    >
    \context StaffGroup = timpani <
      \context Staff = timpani <
	\property Staff.midiInstrument = #"timpani"
	\property Staff.instrument = #'(lines "Timpani" "(C-G)")
	\property Staff.instr = #"Timp."
	\clef bass
        \Key
	\timpani
      >
    >
    \context StaffGroup = brass <
      \context Staff = trombe <
  	\property Staff.midiInstrument = #"trumpet"
	\property Staff.instrument = #`(lines "2 Trombe" "(C)")
	\property Staff.instr = #`(lines "Tbe." "(C)")
        \Key
	\context Voice=one \partcombine Voice
	  \context Thread=one \tromboI
	  \context Thread=two \tromboII
      >
      \context Staff = corni <
        \property Staff.midiInstrument = #"french horn"
	\property Staff.instrument = #`(lines "Corno" (columns "(E" ,text-flat ")"))
	\property Staff.instr = #`(lines "Cor."  (columns "(E" ,text-flat ")"))
	\property Staff.transposing = #3
	\notes \key bes \major
	\context Voice=one \corno
      >
    >
  >
  \paper {
    indent = 15 * \staffspace
    linewidth = 60 * \staffspace
    textheight = 90 * \staffspace
    \translator{
      \VoiceContext
      \consists "Multi_measure_rest_engraver"
    }
    \translator{
      \HaraKiriStaffContext
      \remove "Multi_measure_rest_engraver"
    }
  }
  \midi {
    \tempo 4 = 75
  }
}


