\version "2.19.26"

\header {
  texidoc = "@code{\\markupMap} can be used for applying a markup function
to music properties throughout a music expressions, like the @code{text} of
all contained lyric events."
}

\layout { ragged-right = ##t }

\new Voice
\markupMap TextScriptEvent.text
           #italic-markup
{ g'2^"See" c''^"this?" }
\addlyrics {
  \markupMap text
             \markup \fontsize #5 \with-color #red \rotate #30 \etc
  { Oh yes! }
}
