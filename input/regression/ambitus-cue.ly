\version "2.21.0"

\header {
  texidoc = "Ambitus for pieces beginning with @code{\\cueDuringWithClef}.

Cues are often used at or near the beginning of a piece. Furthermore,
a cue is frequently in a different clef, so the
@code{\\cueDuringWithClef} command is handy.  Using this command at
the beginning of a piece should leave the ambitus displayed based
on the main clef.

An @code{Ambitus_engraver} should ignore notes in @code{CueVoice}
contexts.
"
}

\addQuote "other" \relative { r4 c e g }

\new Staff \new Voice \relative c'
{
 \cueDuringWithClef "other" #UP "bass" { R1 } |
 e4 b c2
}

\layout {
 \context {
   \Staff
   \consists "Ambitus_engraver"
 }
}
