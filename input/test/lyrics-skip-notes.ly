
\header
{
texidoc ="

By inserting @code{\\skip} statements into lyric lines, one can attach
less lyric syllables to a melody. 

"
}


% shorthand for Skip Lyric
sl =  { \skip 4 }

\version "2.3.4"
\score {
  <<
  \context Voice = "A"    {c4 c c c}
  \lyricsto "A" \context Lyrics=A \lyrics { foo __ \sl \sl bar }
  \lyricsto "A" \context Lyrics=B \lyrics { foo -- \sl baz bar }
  \lyricsto "A" \context Lyrics=C \lyrics { foo -- baz -- baaz bar }
  >>
}
