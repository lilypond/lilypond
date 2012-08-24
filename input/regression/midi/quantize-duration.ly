% Lily was here -- automatically converted by ../../../scripts/midi2ly.py from out-www/voice-2.midi
\version "2.16.0"

\layout {
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
    \remove "Rest_engraver"
    \consists "Completion_rest_engraver"
  }
}
\midi {
  \context {
    \Score
    midiChannelMapping = #'instrument
  }
}

% included from ./out-www/voice-2.header
\header {
texidoc="@code{midi2ly}'s option @option{--duration-quant} quantizes durations of notes."
options="--duration-quant=4"
}
% end

trackAchannelA = {
  
  % [SEQUENCE_TRACK_NAME] control track
  
  % [TEXT_EVENT] creator: 
  
  % [TEXT_EVENT] GNU LilyPond 2.13.54          
  
  \time 4/4 
  
  \tempo 4 = 60 
  
}

trackA = <<
  \context Voice = voiceA \trackAchannelA
>>


trackBchannelA = \relative c {
  r4*1/8 c4*7/8 
  c4*7/8 r4*1/8
}

trackB = <<
  \context Voice = voiceA \trackBchannelA
>>


\score {
  <<
    \context Staff=trackB \trackB
  >>
  \layout {}
  \midi {}
}
