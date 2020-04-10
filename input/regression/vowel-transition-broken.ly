\version "2.21.0"
\header
{

  texidoc = "A vowel transition runs to the end of the line if it
continues on the next line, or if the next lyric syllable is at
the first note on the next line.  Transition arrows are printed at
the beginning of the line only when they go past the first note,
or when property @code{after-line-breaking} is @code{#t}."

}

\layout {
  ragged-right = ##t
}

<<
  \new Voice = "A"  {
    a1 ( b1 \break
    a) a2( b) \break
    a1 \break
    a1
  }
  \context Lyrics \lyricsto "A" {
    a \vowelTransition
    b \vowelTransition
    \override VowelTransition.after-line-breaking = ##t
    c \vowelTransition
    d
  }
>>
