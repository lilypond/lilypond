#(ly:set-option 'old-relative)
\version "2.1.19"
% ASCII Art output
%
% Process as:
%
%     lilypond -fas foo.ly
%     as2text foo.as

\include "paper-as5.ly"

\score{
  <<
      \context Staff \notes\relative c'{ 
        c d e f
        \repeat "volta" 2 { g a b c }
        \alternative { { c b a g } { f e d c } }
      }
      \context LyricsVoice \lyrics {
        De eer- ste << { maat } { moet } >>
        \repeat fold 2 { }
        \alternative {
          { en dan twee keer } 
          { een koe- plet _ } 
        }
        en dan nog dit er ach- ter aan
      }
  >>
}
