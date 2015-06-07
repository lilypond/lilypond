% Lily was here -- automatically converted by ../../../scripts/midi2ly.py from out-www/rest.midi
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

% included from ./out-www/rest.header
\header {
texidoc="midi2ly identifies rests"
options=""
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


trackBchannelA = \relative {
  r4 
  \set Staff.instrumentName = "trackB:voiceA"
  a, r4 a 
  | % 2
  
}

trackB = <<

  \clef bass
  
  \context Voice = voiceA \trackBchannelA
>>


\score {
  <<
    \context Staff=trackB \trackA
    \context Staff=trackB \trackB
  >>
  \layout {}
  \midi {}
}
