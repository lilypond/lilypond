
\header
{
texidoc ="

By inserting @code{\\skip} statements into lyric lines, one can put less lyric syllables to a melody. 

"
}


% shorthand for Skip Lyric
sl = \notes { \skip 4 }

\version "2.1.16"
\score {
  <<
  \context Voice = "A"  \notes  {c4 c c c}
  \lyricsto "A" \context LyricsVoice=A \lyrics { foo __ \sl \sl bar }
  \lyricsto "A" \context LyricsVoice=B \lyrics { foo -- \sl baz bar }
  \lyricsto "A" \context LyricsVoice=C \lyrics { foo -- baz -- baaz bar }
  >>
}
