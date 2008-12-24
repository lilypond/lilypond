
%% A simple song in LilyPond
<<
  \relative {
    \clef bass
    d,2 d c4 bes a2 \break
    c2 c d4 f g2
  }
  \addlyrics {
    My first Li -- ly song,
    Not much can go wrong!
  }
>>

%% Optional helper for automatic updating by convert-ly.  May be omitted.
\version "2.12.0"
    
