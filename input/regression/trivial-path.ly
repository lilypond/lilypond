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
