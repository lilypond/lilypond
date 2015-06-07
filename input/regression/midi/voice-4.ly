% Lily was here -- automatically converted by ../../../scripts/midi2ly.py from out-www/voice-4.midi
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

% included from ./out-www/voice-4.header
\header {
texidoc="midi2ly maps four voices nicely on one staff as \voiceOne, \voiceTwo, \voiceThree, \voiceFour"
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
  \voiceOne
  
  \set Staff.instrumentName = ":1"
  <c''' a >2 b 
  | % 2
  
}

trackBchannelB = \relative {
  \voiceThree
  c''4. d8 e4 f 
  | % 2
  
}

trackBchannelC = \relative {
  \voiceFour
  d'1 
  | % 2
  
}

trackBchannelD = \relative {
  \voiceTwo
  c'4 c2 c4 
  | % 2
  
}

trackB = <<
  \context Voice = voiceA \trackBchannelA
  \context Voice = voiceB \trackBchannelB
  \context Voice = voiceC \trackBchannelC
  \context Voice = voiceD \trackBchannelD
>>


\score {
  <<
    \context Staff=trackB \trackA
    \context Staff=trackB \trackB
  >>
  \layout {}
  \midi {}
}
