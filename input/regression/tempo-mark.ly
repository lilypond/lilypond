\version "2.27.6"

\header {
  texidoc = "
  Tempo marks inserted by @code{\\tempoMark} appear above the top most staff.
They are left-aligned.

Tempo marks inserted by @code{\\tempoEndMark}, i.e. internally applying
tweaks setting @code{break-visibility} @code{##(#t #t #f)}, @code{non-musical}
@code{##t} and @code{self-alignment-X} @code{#RIGHT} appear right-aligned at the
end of a measure."
}

#(ly:set-option 'warning-as-error #t)

{
  \key d \major
  \tempoMark "measure-begin"
  b1
  \key bes \major
  \time 8/8
  \clef "alto"
  \break
  \tempoMark "measure-begin"
  c'2
  \tempoMark "inside measure"
  d'
  e'1
  \tempoEndMark "measure-end"
  \break
  \key d \major
  \time 4/4
  \clef "treble"
  f'2
  g'
  a'
  \tempoEndMark "end"
}
