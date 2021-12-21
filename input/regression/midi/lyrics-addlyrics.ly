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
texidoc="Lyrics are preserved"
options=""
}
% end

\score {
  <<
    \relative {
      \key g \major
      \time 6/8
      d''4 b8 c4 a8 | d4 b8 g4
    }
    \addlyrics {
      Girls and \markup \bold boys come | out to play,
    }
  >>
  \layout {}
  \midi {}
}
