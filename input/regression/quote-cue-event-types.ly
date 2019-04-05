\header {
  texidoc = "The @code{cueDuring} and @code{quoteDuring} forms of quotation
use the variables @code{quotedCueEventTypes} and @code{quotedEventTypes} to
determine which events are quoted.  This allows different events to be
quoted for cue notes in comparison to normal quotes.

@code{quotedEventTypes} is also the fallback for cue notes if
@code{quotedCueEventTypes} is not set."
}

\version "2.19.21"

quoteMe = \relative { fis'8 r16-. a8.->
                      \acciaccatura c8 b4:8(-\ff~  b16 c8. b8) }
\addQuote quoteMe \quoteMe

<<
  \new Staff \with { instrumentName = "Quoted Voice" } {
    \quoteMe
  }
  \new Staff \with { instrumentName = "quoteDuring" } {
%     \set Staff.quotedEventTypes = #'(StreamEvent)
    \relative { c'8 \quoteDuring "quoteMe" { s8 s4 s2 } }
  }
  \new Staff \with { instrumentName = "cueDuring" } {
    \relative { c'8 \cueDuring "quoteMe" #UP { r8 r4 r2 } }
  }
>>

<<
  \new Staff \with { instrumentName = "Fallback" } {
    \unset Score.quotedCueEventTypes
    \relative { c'8 \cueDuring "quoteMe" #UP { r8 r4 r2 } }
  }
>>
