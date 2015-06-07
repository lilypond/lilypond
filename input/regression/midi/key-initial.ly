% Lily was here -- automatically converted by ../../../scripts/midi2ly.py from out/initial-key.midi
\version "2.19.21"

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

% included from ./out/initial-key.header
\header {
texidoc="keys work in MIDI, this is d-minor"
options=""
}
% end

trackAchannelA = {


  \key d \minor
    
  % [SEQUENCE_TRACK_NAME] control track
  
  % [TEXT_EVENT] creator: 
  
  % [TEXT_EVENT] GNU LilyPond 2.13.54          
  
  \time 4/4 
  
  \tempo 4 = 60 
  
}

trackA = <<
  \context Voice = voiceA \trackAchannelA
>>


trackBchannelA = \relative {
  
  \set Staff.instrumentName = "trackB:voiceA"
  d'4 e f g 
  | % 2
  a bes cis d
  | % 3
  
}

trackB = <<
  \context Voice = voiceA \trackBchannelA
>>


trackCchannelA = {
  
  \set Staff.instrumentName = "trackB:"
  

  \key d \minor
  
}

trackC = <<
  \context Voice = voiceA \trackCchannelA
>>


\score {
  <<
    \context Staff=trackB \trackA
    \context Staff=trackB \trackB
  >>
  \layout {}
  \midi {}
}
