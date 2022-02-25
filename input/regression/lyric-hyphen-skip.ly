\version "2.23.3"

\header {
  texidoc = "A lyric hyphen or vowel transition may occur anywhere in a sequence
of skips.  It spans the entire sequence."
}

\lyrics { x \vowelTransition a -- _ _ b -- _ _ c }
\lyrics { x a _ -- _ b \vowelTransition _ _ c }
\lyrics { a _ -- _ b _ _ -- c }

#(ly:expect-warning
  (G_ "this hyphen or vowel transition was overridden by a later one"))
\lyrics { a -- _ _ \vowelTransition b }

#(ly:expect-warning
  (G_ "this hyphen or vowel transition was overridden by a later one"))
\lyrics { a \vowelTransition _ -- _ b }

#(ly:expect-warning
  (G_ "hyphen or vowel transition has no syllable to attach to on its \
left; removing it"))
\lyrics { _ -- a -- b }
