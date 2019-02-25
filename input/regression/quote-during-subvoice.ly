\version "2.21.0"

\header {
  texidoc = "@code{\\quoteDuring} and @code{\\cueDuring} shall properly quote
voices that create a sub-voice.  The sub-voice will not be quoted, though.
Exceptions are sections of parallel music @code{<< @{...@} \\ @{...@} >>},
which will be quoted.
"
}

% Simple case, normal sub-voice
quoteMe = \relative {
  c'4 c
  \new Voice {
    c4 c
  }
}
\addQuote "quoteMe" \quoteMe
% Also works if wrapped with \new Voice
\addQuote "quoteMeA" \new Voice \quoteMe

% Also works with voice directly inside relative
quoteMeI = \relative \new Voice {
  c'4 c4
}
\addQuote "quoteMeI" \quoteMeI

% Quoting music with some parallel sections (identical rhythm)
quoteMeII = \relative {
  c'4 c
  << { d4 e4 } \\ { c4 b4 } >>
  c4
}
\addQuote "quoteMeII" \quoteMeII

% Quoting music with some parallel sections (different rhythm)
quoteMeIII = \relative {
  c'4 c
  << { d4 e4 } \\ { c4. b8 } >>
  c4
}
\addQuote "quoteMeIII" \quoteMeIII




<<
  \new Staff \relative {
    c''4 \cueDuring "quoteMe" #DOWN { r4 }
    c4 \cueDuring "quoteMe" #DOWN { r4 } % <- no cue note due to sub-voice
  }
  \new Staff \relative {
    c''4 \cueDuring "quoteMeA" #DOWN { r4 }
    c4 \cueDuring "quoteMeA" #DOWN { r4 } % <- no cue note due to sub-voice
  }
  \new Staff \relative {
    c''4 \cueDuring "quoteMeI" #DOWN { r4 }
    c4
  }
  \new Staff \relative {
    c''4 \cueDuring "quoteMeII" #DOWN { r4 }
    c4 \cueDuring "quoteMeII" #DOWN { r4 } % <- quoted parallel notes
  }
  \new Staff \relative {
    c''4 \cueDuring "quoteMeIII" #DOWN { r4 }
    c4 \cueDuring "quoteMeIII" #DOWN { r4 } % <- quoted parallel notes
  }
>>
