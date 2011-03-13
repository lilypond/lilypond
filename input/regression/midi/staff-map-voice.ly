\version "2.13.53"

\header {
texidoc="Midi2ly remaps voices correctly to staves in MIDI-files that use voice<->channel mapping when combined with staff<->track mapping.  TODO: pianostaff"
options=""
}

\score {
%% TODO:PIANOSTAFF  \context PianoStaff <<
  <<
    \context Staff = "treble" <<
      \set Score.midiChannelMapping = #'voice
      \context Voice="one" \relative c'' {
	\time 4/4
	\key c \minor
	\voiceOne
%comes
%7
	f8 es16 d c16 bes ! as g f8 as' g f 
%8	es8 d es f b, c d b |
	f,16 g as4 g16 f e2 |
      }
      \context Voice="two" \relative c'' {
	\voiceTwo
%dux
%7
	c4 r4 r8 f es d |
%8	r8 as g f g f16 es f8 d | 
	<b, d>8 r <b d> r <g c>2 |
      }
    >>
    \context Staff = "bass" <<
      \context Voice="three" \relative c' {
	\key c \minor
	\clef bass
%7
	r8 c16 b c8 g as c16 b c8 d |
%8	g8 c16 b c8 d f,16 g as4 g16 f | 
	<c,, c'>1
      }
    >>
  >>
  \layout {}
  \midi {}
}
