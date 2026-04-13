\version "2.25.35"

\header {
  texidoc = "@code{Hairpin} grobs do not collide with @code{SpanBar} grobs.
@code{Hairpin} grobs should, however, go to the end of a line when the
@code{SpanBar} is not present.
"
}

\score {
  <<
    \new GrandStaff <<
      \new Staff \relative { a'\< a a a \break a a a a \break a a a a\! }
      \new Staff \relative { a'4 a a a s1 a4 a a a }
    >>
    \new GrandStaff <<
      \new Staff \relative { a'^\< a a a a a a a a a a a\! }
      \new Staff \relative { \*12 a'4 }
    >>
    \new GrandStaff <<
      \new Staff \relative { a'4 a a a s1 a4 a a a }
      \new Staff \relative { a'^\< a a a a a a a a a a a\! }
    >>
  >>
  \layout {
    \context {
      \Staff
      \RemoveEmptyStaves
    }
  }
}