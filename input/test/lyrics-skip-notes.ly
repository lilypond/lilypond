
\header
{
    texidoc ="

By inserting @code{\\skip} statements into lyric lines, one can attach
less lyric syllables to a melody. 

"
}


				% shorthand for Skip Lyric
sl =  { \skip 4 }

\version "2.3.8"
\score {
    <<
	\context Voice = "A" \relative { c4 c c c }
	\lyricsto "A" \new Lyrics { foo __ \sl \sl bar }
	\lyricsto "A" \new Lyrics { foo -- \sl baz bar }
	\lyricsto "A" \new Lyrics { foo -- baz -- baaz bar }
    >>
}
