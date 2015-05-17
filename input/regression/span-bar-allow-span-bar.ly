
\version "2.19.21"

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
        \relative {
          c''2 c c c
          \once \override Staff.BarLine.allow-span-bar = ##f
          c2 c c c
          c2 c c c
          \once \override Staff.BarLine.allow-span-bar = ##f
          c2 c c c
        }
    \new Lyrics \lyricsto "upper" {
      long-syllable a b c long-syllable a b c
      long-syllable a b c long-syllable a b c
    }

    \new Staff
      \new Voice = "middle"
        \relative {
          c''2 c c c
          c2 c c c
          c2 c c c
          \once \override Staff.BarLine.allow-span-bar = ##f
          c2 c c c
        }
    \new Lyrics \lyricsto "middle" {
      syllable a b c syllable a b c
      syllable a b c syllable a b c
    }

    \new Staff
      \new Voice = "lower"
        \relative {
          c''2 c c c
          c2 c c c
          c2 c c c
          c2 c c c
        }
    \new Lyrics \lyricsto "lower" {
      word a b c word a b c
      word a b c word a b c
    }
  >>
>>
