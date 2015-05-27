\version "2.19.22"

\header {
  texidoc = "@code{make-relative} has to copy its argument expressions
in case the generated music expression is getting copied and modified.

The code here defines a @code{\\reltranspose} function working inside
of @code{\\relative} and uses it.  Both staves should appear
identical."
}

\layout {
  ragged-right = ##t
}

reltranspose =
#(define-music-function (from to music)
  (ly:pitch? ly:pitch? ly:music?)
  (make-relative (music) music
   #{ \transpose #from #to $music #}))

mus =
\reltranspose c g {
  \partial 4. c8 e g |
  c2 r8 c, e g c1 | \bar "|."
}

<<
  \new Staff \relative \mus
  \new Staff \relative \mus
>>
