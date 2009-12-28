\header {


texidoc = " The @code{cueDuring} and @code{quoteDuring} forms of quotation
will use the variables @code{quotedCueEventTypes} and @code{quotedEventTypes}
to determine which events are quoted. This allows different events to be
quoted for cue notes than for normal quotes.

@code{quotedEventTypes} is also the fallback for cue notes if
@code{quotedCueEventTypes} is not set."

}

\version "2.13.10"

quoteMe = \relative c' { fis8 r16-. a8.-> \acciaccatura c8 b4(-\ff~  b16 c8. b8) }
\addQuote quoteMe \quoteMe

<<
  \new Staff \with { instrumentName = "Quoted Voice" } {
    \quoteMe
  }
  \new Staff \with { instrumentName = "quoteDuring" } {
%     \set Staff.quotedEventTypes = #'(StreamEvent)
    \relative c' { c8 \quoteDuring "quoteMe" { s8 s4 s2 } }
  }
  \new Staff \with { instrumentName = "cueDuring" } {
    \relative c' { c8 \cueDuring "quoteMe" #UP { r8 r4 r2 } }
  }
>>

<<
  \new Staff \with { instrumentName = "Fallback" } {
    \unset Score.quotedCueEventTypes
    \relative c' { c8 \cueDuring "quoteMe" #UP { r8 r4 r2 } }
  }
>>
