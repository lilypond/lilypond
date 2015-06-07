\version "2.19.21"

\header {
texidoc="Midi2ly remaps voices correctly to staves in MIDI-files that use instrument<->channel mapping when combined with voice<->track mapping.  TODO: pianostaff"
options=""
}

\score {
%% TODO:PIANOSTAFF  \context PianoStaff <<
  <<
    \context Staff = "treble" <<
      \set Score.midiChannelMapping = #'instrument
      \context Voice="one" \relative {
	\time 4/4
	\key c \minor
	\voiceOne
%comes
%7
	f''8 es16 d c16 bes ! as g f8 as' g f 
%8	es8 d es f b, c d b |
	f,16 g as4 g16 f e2 |
      }
      \context Voice="two" \relative {
	\voiceTwo
%dux
%7
	c''4 r4 r8 f es d |
%8	r8 as g f g f16 es f8 d | 
	<b, d>8 r <b d> r <g c>2 |
      }
    >>
    \context Staff = "bass" <<
      \context Voice="three" \relative {
	\key c \minor
	\clef bass
%7
	r8 c'16 b c8 g as c16 b c8 d |
%8	g8 c16 b c8 d f,16 g as4 g16 f | 
	<c,, c'>1
      }
    >>
  >>
  \layout {}
  \midi {}
}
