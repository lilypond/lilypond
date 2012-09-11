
\version "2.16.0"

\header {
  texidoc = "The @code{SpanBarStub} grob takes care of horizontal spacing
for @code{SpanBar} grobs.  When the @code{SpanBar} is disallowed, objects
in contexts that the span bar would have otherwise crossed align as if the
span bar were not there.
"
}

<<
  \new Staff {
    \repeat unfold 64 { c''8 }
  }
  \new GrandStaff <<
    \new Staff
      \new Voice = "upper"
        \relative c'' {
          c2 c c c
          \once \override Staff . BarLine #'allow-span-bar = ##f
          c2 c c c
          c2 c c c
          \once \override Staff . BarLine #'allow-span-bar = ##f
          c2 c c c
        }
    \new Lyrics \lyricsto "upper" \lyricmode {
      long-syllable a b c long-syllable a b c
      long-syllable a b c long-syllable a b c
    }

    \new Staff
      \new Voice = "middle"
        \relative c'' {
          c2 c c c
          c2 c c c
          c2 c c c
          \once \override Staff . BarLine #'allow-span-bar = ##f
          c2 c c c
        }
    \new Lyrics \lyricsto "middle" \lyricmode {
      syllable a b c syllable a b c
      syllable a b c syllable a b c
    }

    \new Staff
      \new Voice = "lower"
        \relative c'' {
          c2 c c c
          c2 c c c
          c2 c c c
          c2 c c c
        }
    \new Lyrics \lyricsto "lower" \lyricmode {
      word a b c word a b c
      word a b c word a b c
    }
  >>
>>
