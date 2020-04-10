\version "2.21.0"
\header
{

  texidoc = "For vowel transitions, left/right padding are
independent of left-broken/right-broken padding."

}

\layout {
  ragged-right = ##t
  indent = #0
}

music = {
  r2 a ~ \break
  a2 a \bar "||" \break
}

words = \lyricmode {
  ah \vowelTransition
  oh
}

<<
  \new Voice = "A"  {
    \music
    \music
    \music
    r2 a \break
    a2 r2 \bar "|."
  }
  \new Lyrics \lyricsto "A" {
    \override VowelTransition.minimum-length = #4
    \words
    \once \override VowelTransition.bound-details.left.padding = #2
    \once \override VowelTransition.bound-details.right.padding = #4
    \words
    \once \override VowelTransition.bound-details.right-broken.padding = #2
    \once \override VowelTransition.bound-details.left-broken.padding = #4
    \words
    \once \override VowelTransition.after-line-breaking = ##t
    \once \override VowelTransition.bound-details.left.padding = #2
    \once \override VowelTransition.bound-details.right-broken.padding = #2
    \once \override VowelTransition.bound-details.left-broken.padding = #4
    \once \override VowelTransition.bound-details.right.padding = #4
    \words
  }
>>
