\version "2.23.11"

\header {
  texidoc = "Paths can be empty, or contain just
a @code{moveto} command.  The extents of such a
path are empty."
}

%% Test emptiness of extents: the Xs should
%% be aligned regularly.

\markup \line { X X X \path #0.5 #'() X}

\markup \line { X X X \path #0.5 #'((moveto 0 0)) X }

{
  <<
    { b'4 a'4_\markup \line { x x \path #0.5 #'() x} 4 b'4 }
    { a'4 b'4^\markup \line { x x \path #0.5 #'((moveto 0 0)) x } 4 a'4 }
  >>
}
