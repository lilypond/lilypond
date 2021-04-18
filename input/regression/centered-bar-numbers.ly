\version "2.23.3"

\header {
  texidoc = "Bar numbers may be centered within their measure."
}

\layout {
  \context {
    \Score
    centerBarNumbers = ##t
  }
}

{
  c'1 1 1 1
  % It's unlikely that anyone will really alternate centered and
  % normal bar numbers, but there is no reason not to support it,
  % and we should at least ensure no crash.
  \set Score.centerBarNumbers = ##f
  \override Score.BarNumber.break-visibility = ##(#t #t #t)
  1 1 1
  \set Score.centerBarNumbers = ##t
  1 1
}