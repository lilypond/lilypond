\version "2.25.35"

\header {
  texidoc = "The order of break-align symbols may be changed
with @code{\\breakAlignInsert}."
}

\layout {
  ragged-last = ##t
}

mus = {
  \key e \major
  a'1
  \key f \minor
  \time 3/4
  c''2.
  \key e \major
  \time 4/4
  \break
  a'1
  \time 2/4
  \key f \minor
}

{
  <>^"first: time-signature before clef"
  <>_"then: clef after key-signature"
  \breakAlignInsert time-signature before clef
  \breakAlignInsert clef after key-signature
  \override Score.TimeSignature.space-alist.key-signature = #'(minimum-space . 3)
  \override Score.KeySignature.space-alist.clef = #'(minimum-space . 6)
  \mus
}

{
  \override Score.TimeSignature.space-alist.clef = #'(minimum-space . 3)
  <>^"first: clef after key-signature"
  <>_"then: time-signature before clef"
  \breakAlignInsert clef after key-signature
  \breakAlignInsert time-signature before clef
  \mus
}

{
  <>^"key-cancellation before staff-bar at center-of-line"
  \breakAlignInsert #center-visible key-cancellation before staff-bar
  \mus
}

{
  <>^"key-cancellation before staff-bar at end-of-line"
  \breakAlignInsert #end-of-line-visible key-cancellation before staff-bar
  \mus
}
