
\header
{
texidoc = "Rests in collisions sit opposite of the note if no direction is
  specified for the voice containing the rest."
}


\score{\notes\relative c''
       \context Staff <
\time 2/4
\context Voice=upper {
    r4 }
\context Voice=lower {\voiceTwo
b8  }>}
