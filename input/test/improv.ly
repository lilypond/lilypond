\version "1.9.8"
\header {

texidoc =


"@cindex improvisation


Noteheads for improvisation have a different shape.


" }


improOn = \notes {\translator Voice = impro }
improOff = \notes {\translator SquashVoice = melo }

global = \notes { s1*3 \bar "|." }

\score {
  <<
    \context ChordNames \chords {e8*7:m7 a2.:m7 bes4:m7 b1:m7 e8:m }
    \context Staff \notes <<
      \context SquashVoice = impro { \global }
      \context Voice = melo \transpose c c' {
	e8 e g a a16(bes)(a8) g \improOn e8
	~e2~e8 f4 fis8
	~fis2 \improOff a16(bes) a8 g e
      }
    >>
  >>
  \paper { 
    \translator {
      \VoiceContext
      \name SquashVoice
      \alias Voice
      \consists Pitch_squash_engraver
      squashedPosition = #0
      NoteHead \override #'style = #'slash
    }

    \translator {
      \VoiceContext
      \alias SquashVoice
    }
    
    \translator {
      \StaffContext
      \remove "Accidental_engraver"
      \accepts SquashVoice
    }
    raggedright = ##t
  }
}

