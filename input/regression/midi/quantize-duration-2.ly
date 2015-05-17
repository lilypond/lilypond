% Lily was here -- automatically converted by out/bin/midi2ly from /home/janneke/testmidi.mid
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

\header {
texidoc="@code{midi2ly}'s option @option{--duration-quant} preserves first note length (16)."
options="--duration-quant=16"
}

trackAchannelA = {


  \key a \major
    
  % [TEXT_EVENT] Nokia Tune
  
  \tempo 4 = 120 
  

  \key a \major
  
  \time 3/8 
  
}

trackA = <<
  \context Voice = voiceA \trackAchannelA
>>


trackBchannelB = \relative {
  e'''4*41/192 r4*7/192 d4*41/192 r4*7/192 fis,4*89/192 r4*7/192 gis4*89/192 
  r4*7/192 
  | % 2
  cis4*41/192 r4*7/192 b4*41/192 r4*7/192 d,4*89/192 r4*7/192 e4*89/192 
}

trackB = <<
  \context Voice = voiceA \trackBchannelB
>>


\score {
  <<
    \context Staff=trackB \trackA
    \context Staff=trackB \trackB
  >>
  \layout {}
  \midi {}
}
