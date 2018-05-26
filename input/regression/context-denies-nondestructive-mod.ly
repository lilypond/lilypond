\version "2.19.80"

\header {
  texidoc = "A @code{\\denies} statement in a @code{\\with} block
applies to the local context only; it does not change the global
context definition.  The lower staff should hold a B-sharp."
}

\layout {
  \context {
    \Staff
    \remove "Accidental_engraver"
    \accepts "AdHocPaddingOne"
    \accepts "AdHocVoice" % important: this is not first in the accepts list
    \accepts "AdHocPaddingTwo"
  }

  \context {
    \Voice
    \name "AdHocVoice"
    \alias Voice
    \consists "Accidental_engraver"
  }
}

\new Score <<
  \new Staff \with { \denies "AdHocVoice" } <<
    R1
  >>
  \new Staff <<
    %% If the \denies in the upper staff were wrongly carried over to
    %% this one, LilyPond would refuse to create an AdHocVoice and no
    %% accidental would be engraved.
    \new AdHocVoice bis'1
  >>
>>
